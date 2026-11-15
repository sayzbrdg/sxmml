/*
Copyright (c) 2026 Seiji Ohashi <sayzbrdg@gmail.com>
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

 1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.
 3. Neither the name of the authors nor the names of its contributors
    may be used to endorse or promote products derived from this
    software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
/* SPDX-License-Identifier: BSD-3-Clause */
/* Scheme 版の探索順と窓幅による区間 DP を維持する。 */
#include "sxmml_repeat_tree.h"
#include <stdlib.h>
#include <string.h>

typedef struct Allocation {
    struct Allocation *next;
    uint64_t data[];
} Allocation;
struct RtRun { Allocation *allocations; };
typedef struct { uint64_t fee; int allowed; } FeeEntry;
typedef struct {
    size_t n, span;
    const uint64_t *sums;
    size_t *lcp, *roots;
    uint64_t *costs;
    RtNode *nodes;
    size_t used;
} Solver;

RtRun *rt_run_new(void) { return calloc(1, sizeof(RtRun)); }

void *rt_run_alloc(RtRun *run, size_t bytes)
{
    if (bytes > SIZE_MAX - sizeof(Allocation)) return NULL;
    Allocation *a = malloc(sizeof(Allocation) + bytes);
    if (!a) return NULL;
    a->next = run->allocations;
    run->allocations = a;
    return a->data;
}

void rt_run_free(RtRun *run)
{
    if (!run) return;
    Allocation *a = run->allocations;
    while (a) {
        Allocation *next = a->next;
        free(a);
        a = next;
    }
    free(run);
}

static void *array(RtRun *run, size_t count, size_t width, RtStatus *status)
{
    if (*status != RT_OK) return NULL;
    if (count > (SIZE_MAX - sizeof(Allocation)) / width) {
        *status = RT_SIZE_RANGE;
        return NULL;
    }
    void *p = rt_run_alloc(run, count * width);
    if (!p) *status = RT_NOMEM;
    return p;
}

static size_t cell(const Solver *s, size_t i, size_t m)
{ return i * s->span + m - 1; }
static uint64_t cost(const Solver *s, size_t i, size_t m)
{ return s->costs[cell(s, i, m)]; }
static size_t lcp(const Solver *s, size_t i, size_t j)
{ return j == s->n ? 0 : s->lcp[i * s->span + j - i]; }

static size_t node(Solver *s, RtNodeKind kind, size_t a, size_t b, size_t count)
{
    size_t index = s->used++;
    s->nodes[index] = (RtNode){ kind, a, b, count };
    return index;
}

/* 子を先に格納し、Gauche 側の変換も添字順に進められるようにする。 */
static size_t restore(Solver *s, size_t i, size_t m)
{
    size_t root = s->roots[cell(s, i, m)], x = root / 4;
    switch (root % 4) {
    case 0: {
        size_t result = node(s, RT_LITERAL, i, 0, 0);
        for (size_t j = i + 1; j < i + m; j++) {
            size_t right = node(s, RT_LITERAL, j, 0, 0);
            result = node(s, RT_CONCAT, result, right, 0);
        }
        return result;
    }
    case 1: {
        size_t left = restore(s, i, x);
        size_t right = restore(s, i + x, m - x);
        return node(s, RT_CONCAT, left, right, 0);
    }
    case 2: {
        size_t body = restore(s, i, x);
        return node(s, RT_REPEAT, body, 0, m / x);
    }
    default: {
        size_t p = x / s->span, a = x % s->span;
        size_t front = restore(s, i, a);
        size_t back = restore(s, i + a, p - a);
        return node(s, RT_FINAL, front, back, m / p + 1);
    }
    }
}

RtStatus rt_compress(RtRun *run, const uint64_t *weights, size_t n,
                     const RtSettings *settings, RtSameFn same,
                     void *context, RtResult *result)
{
    RtStatus status = RT_OK;
    size_t span = settings->window < n ? settings->window : n;
    if (!n || !span || n == SIZE_MAX || n > SIZE_MAX / span
        || n > SIZE_MAX / 2) return RT_SIZE_RANGE;
    size_t cells = n * span;
    Solver s = { .n = n, .span = span };
    uint64_t *sums = array(run, n + 1, sizeof(*sums), &status);
    s.lcp = array(run, cells, sizeof(*s.lcp), &status);
    s.costs = array(run, cells, sizeof(*s.costs), &status);
    s.roots = array(run, cells, sizeof(*s.roots), &status);
    FeeEntry *repeat = array(run, n + 1, sizeof(*repeat), &status);
    FeeEntry *final = settings->allow_final
        ? array(run, n + 1, sizeof(*final), &status) : NULL;
    uint64_t *chain = array(run, n + 1, sizeof(*chain), &status);
    size_t *from = array(run, n + 1, sizeof(*from), &status);
    s.nodes = array(run, 2 * n - 1, sizeof(*s.nodes), &status);
    size_t *blocks = array(run, n, sizeof(*blocks), &status);
    if (status != RT_OK) return status;
    s.sums = sums;
    sums[0] = 0;
    for (size_t i = 0; i < n; i++) {
        if (weights[i] > UINT64_MAX - sums[i]) return RT_COST_RANGE;
        sums[i + 1] = sums[i] + weights[i];
    }
    memset(s.lcp, 0, cells * sizeof(*s.lcp));
    memset(repeat, 0, (n + 1) * sizeof(*repeat));
    if (final) memset(final, 0, (n + 1) * sizeof(*final));
    for (size_t i = 0; i < settings->repeat_count; i++) {
        RtFee entry = settings->repeat_fees[i];
        if (entry.count <= n) repeat[entry.count] = (FeeEntry){ entry.fee, 1 };
    }
    for (size_t i = 0; final && i < settings->final_count; i++) {
        RtFee entry = settings->final_fees[i];
        if (entry.count <= n) final[entry.count] = (FeeEntry){ entry.fee, 1 };
    }
    for (size_t u = n - 1; u-- > 0;) {
        for (size_t d = 1; d < span && d < n - u; d++) {
            if (same(context, u, u + d))
                s.lcp[u * span + d] = 1 + lcp(&s, u + 1, u + d + 1);
        }
    }
    for (size_t m = 1; m <= span; m++) {
        for (size_t i = 0; i <= n - m; i++) {
            uint64_t best = sums[i + m] - sums[i];
            size_t root = 0;
            for (size_t k = 1; k < m; k++) {
                uint64_t value = cost(&s, i, k) + cost(&s, i + k, m - k);
                if (value < best) { best = value; root = 1 + 4 * k; }
            }
            for (size_t p = 1; p < m; p++) {
                size_t t = m / p, a = m % p;
                if (lcp(&s, i, i + p) < m - p) continue;
                if (!a) {
                    FeeEntry fee = repeat[t];
                    if (fee.allowed && fee.fee < best) {
                        uint64_t body = cost(&s, i, p);
                        if (body < best - fee.fee) {
                            best = body + fee.fee;
                            root = 2 + 4 * p;
                        }
                    }
                } else if (final) {
                    FeeEntry fee = final[t + 1];
                    if (fee.allowed && fee.fee < best) {
                        uint64_t body = cost(&s, i, a) + cost(&s, i + a, p - a);
                        if (body < best - fee.fee) {
                            best = body + fee.fee;
                            root = 3 + 4 * (p * span + a);
                        }
                    }
                }
            }
            s.costs[cell(&s, i, m)] = best;
            s.roots[cell(&s, i, m)] = root;
        }
    }
    chain[0] = 0;
    for (size_t end = 1; end <= n; end++) {
        size_t first = end > span ? end - span : 0;
        uint64_t best = chain[first] + cost(&s, first, end - first);
        from[end] = first;
        for (size_t i = first + 1; i < end; i++) {
            uint64_t value = chain[i] + cost(&s, i, end - i);
            if (value < best) { best = value; from[end] = i; }
        }
        chain[end] = best;
    }
    size_t end = n, count = 0;
    while (end) { blocks[count++] = end; end = from[end]; }
    size_t root = restore(&s, from[blocks[count - 1]], blocks[count - 1]);
    while (--count) {
        end = blocks[count - 1];
        size_t next = restore(&s, from[end], end - from[end]);
        root = node(&s, RT_CONCAT, root, next, 0);
    }
    *result = (RtResult){ chain[n], s.nodes, s.used, root };
    return RT_OK;
}
