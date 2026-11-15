/* SPDX-License-Identifier: BSD-3-Clause */
#ifndef SXMML_NATIVE_H
#define SXMML_NATIVE_H
#include <gauche.h>
#include <gauche/extend.h>
#include "sxmml_repeat_tree.h"
SCM_DECL_BEGIN
ScmObj SxmmlNativeBinding(const char *, const char *);
ScmObj SxmmlNativeCompress(ScmObj, ScmObj, ScmObj, RtSameFn, void *);
ScmObj SxmmlNativeSchemeCompress(ScmObj, ScmObj, ScmObj, ScmObj);
SCM_DECL_END
#endif
