/* SPDX-License-Identifier: BSD-3-Clause */
/* Gauche の値の変換と、巻き戻しを含む C の領域の寿命を担当する。 */
#include "sxmml_native.h"
#include <stdlib.h>

ScmObj SxmmlNativeBinding(const char *module, const char *name)
{
    ScmModule *mod = SCM_MODULE(SCM_FIND_MODULE(module, FALSE));
    if (!mod) Scm_Error("module is not loaded: %s", module);
    ScmObj value = Scm_GlobalVariableRef(mod, SCM_SYMBOL(SCM_INTERN(name)), 0);
    if (SCM_UNBOUNDP(value)) Scm_Error("binding is not defined: %s#%s", module, name);
    return value;
}

static uint64_t integer(ScmObj value, const char *name, uint64_t limit)
{
    int out = 0;
    uint64_t result = Scm_GetIntegerU64Clamp(value, SCM_CLAMP_NONE, &out);
    if (out || result > limit)
        Scm_Error("repeat-tree: %s out of range: %S (expected an exact integer <= %S)",
                  name, value, Scm_MakeIntegerU64(limit));
    return result;
}

static void *allocate(RtRun *run, size_t count, size_t width)
{
    if (count > SIZE_MAX / width)
        Scm_Error("repeat-tree: workspace size out of range: %S x %S (expected a byte count <= %S)",
                  Scm_MakeIntegerU64(count), Scm_MakeIntegerU64(width), Scm_MakeIntegerU64(SIZE_MAX));
    void *result = rt_run_alloc(run, count * width);
    if (!result) Scm_Error("repeat-tree: could not allocate workspace");
    return result;
}

static RtFee *fees(RtRun *run, ScmObj entries, size_t *count)
{
    *count = (size_t)Scm_Length(entries);
    RtFee *result = allocate(run, *count, sizeof(*result));
    size_t i = 0;
    SCM_FOR_EACH(entries, entries) {
        ScmObj pair = SCM_CAR(entries);
        result[i].count = (size_t)integer(SCM_CAR(pair), "repeat count", SIZE_MAX);
        result[i++].fee = integer(SCM_CDR(pair), "control fee", UINT64_MAX);
    }
    return result;
}

static ScmObj tree_to_scheme(const RtResult *result)
{
    const char *names[] = { "make-literal", "make-concat", "make-repeat", "make-final" };
    ScmObj constructors[4];
    for (size_t i = 0; i < 4; i++)
        constructors[i] = SxmmlNativeBinding("sxmml.repeat-tree", names[i]);
    ScmObj *nodes = SCM_NEW_ARRAY(ScmObj, result->node_count);
    for (size_t i = 0; i < result->node_count; i++) {
        const RtNode *node = &result->nodes[i];
        switch (node->kind) {
        case RT_LITERAL:
            nodes[i] = Scm_ApplyRec1(constructors[0], Scm_MakeIntegerU64(node->a));
            break;
        case RT_CONCAT:
            nodes[i] = Scm_ApplyRec2(constructors[1], nodes[node->a], nodes[node->b]);
            break;
        case RT_REPEAT:
            nodes[i] = Scm_ApplyRec2(constructors[2], Scm_MakeIntegerU64(node->count), nodes[node->a]);
            break;
        case RT_FINAL:
            nodes[i] = Scm_ApplyRec3(constructors[3], Scm_MakeIntegerU64(node->count),
                                    nodes[node->a], nodes[node->b]);
            break;
        }
    }
    return nodes[result->root];
}

ScmObj SxmmlNativeCompress(ScmObj items, ScmObj weights, ScmObj settings,
                           RtSameFn same, void *context)
{
    RtRun *run = rt_run_new();
    if (!run) Scm_Error("repeat-tree: could not allocate workspace");
    ScmObj tree = SCM_UNDEFINED, total = SCM_UNDEFINED;
    SCM_UNWIND_PROTECT {
        size_t n = SCM_VECTOR_SIZE(items);
        uint64_t *values = allocate(run, n, sizeof(*values));
        for (size_t i = 0; i < n; i++)
            values[i] = integer(SCM_VECTOR_ELEMENT(weights, i), "weight", UINT64_MAX);
        RtSettings options;
        ScmObj proc = SxmmlNativeBinding("sxmml.repeat-tree", "repeat-tree-settings-repeat-costs");
        options.repeat_fees = fees(run, Scm_ApplyRec1(proc, settings), &options.repeat_count);
        proc = SxmmlNativeBinding("sxmml.repeat-tree", "repeat-tree-settings-final-costs");
        options.final_fees = fees(run, Scm_ApplyRec1(proc, settings), &options.final_count);
        proc = SxmmlNativeBinding("sxmml.repeat-tree", "repeat-tree-settings-allow-final?");
        options.allow_final = SCM_TRUEP(Scm_ApplyRec1(proc, settings));
        proc = SxmmlNativeBinding("sxmml.repeat-tree", "repeat-tree-settings-window");
        options.window = (size_t)integer(Scm_ApplyRec1(proc, settings), "window", SIZE_MAX);
        RtResult result;
        RtStatus status = rt_compress(run, values, n, &options, same, context, &result);
        if (status == RT_COST_RANGE) {
            uint64_t sum = 0;
            for (size_t i = 0; i < n; i++) {
                if (values[i] > UINT64_MAX - sum)
                    Scm_Error("repeat-tree: total weight out of range: %S + %S (expected a total <= 18446744073709551615)",
                              Scm_MakeIntegerU64(sum), Scm_MakeIntegerU64(values[i]));
                sum += values[i];
            }
        }
        if (status == RT_SIZE_RANGE)
            Scm_Error("repeat-tree: workspace size out of range: %S x %S (expected a byte count <= %S)",
                      Scm_MakeIntegerU64(n), Scm_MakeIntegerU64(options.window), Scm_MakeIntegerU64(SIZE_MAX));
        if (status == RT_NOMEM) Scm_Error("repeat-tree: could not allocate workspace");
        total = Scm_MakeIntegerU64(result.cost);
        tree = tree_to_scheme(&result);
    } SCM_WHEN_ERROR {
        rt_run_free(run);
        SCM_NEXT_HANDLER;
    } SCM_END_PROTECT;
    rt_run_free(run);
    return Scm_Values2(total, tree);
}

typedef struct { ScmObj items, same; } SchemeComparison;
static int scheme_same(void *data, size_t left, size_t right)
{
    SchemeComparison *context = data;
    ScmObj result = Scm_ApplyRec2(context->same,
                                SCM_VECTOR_ELEMENT(context->items, left),
                                SCM_VECTOR_ELEMENT(context->items, right));
    if (!SCM_BOOLP(result))
        Scm_Error("repeat-tree: the comparison must return a boolean: %S", result);
    return SCM_TRUEP(result);
}

ScmObj SxmmlNativeSchemeCompress(ScmObj items, ScmObj weights, ScmObj same, ScmObj settings)
{
    SchemeComparison context = { items, same };
    return SxmmlNativeCompress(items, weights, settings, scheme_same, &context);
}

extern void Scm_Init_sxmml_nativelib(void);
extern void Scm_Init_sxmml_optimizerlib(void);
void Scm_Init_sxmml_native(void)
{
    SCM_INIT_EXTENSION(sxmml_native);
    Scm_Init_sxmml_nativelib();
    Scm_Init_sxmml_optimizerlib();
}
