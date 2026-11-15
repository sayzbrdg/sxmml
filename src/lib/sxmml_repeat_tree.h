/* SPDX-License-Identifier: BSD-3-Clause */
#ifndef SXMML_REPEAT_TREE_H
#define SXMML_REPEAT_TREE_H
#include <stddef.h>
#include <stdint.h>

typedef int (*RtSameFn)(void *, size_t, size_t);
typedef enum { RT_OK, RT_COST_RANGE, RT_SIZE_RANGE, RT_NOMEM } RtStatus;
typedef struct RtRun RtRun;
typedef struct { size_t count; uint64_t fee; } RtFee;
typedef struct {
    const RtFee *repeat_fees, *final_fees;
    size_t repeat_count, final_count, window;
    int allow_final;
} RtSettings;
typedef enum { RT_LITERAL, RT_CONCAT, RT_REPEAT, RT_FINAL } RtNodeKind;
typedef struct { RtNodeKind kind; size_t a, b, count; } RtNode;
typedef struct {
    uint64_t cost;
    const RtNode *nodes;
    size_t node_count, root;
} RtResult;

/* 取得した領域と結果は、run を解放するまで有効。 */
RtRun *rt_run_new(void);
void *rt_run_alloc(RtRun *, size_t);
void rt_run_free(RtRun *);
RtStatus rt_compress(RtRun *, const uint64_t *, size_t, const RtSettings *,
                     RtSameFn, void *, RtResult *);
#endif
