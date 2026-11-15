;; 汎用圧縮入口の C 接続。型や構築子はロード時に取得しない。
(in-module sxmml.native)
(inline-stub
 (.include "sxmml_native.h")
 (define-cproc sxmml-native-repeat-tree-compress
   (items::<vector> weights::<vector> same? settings)
   (return (SxmmlNativeSchemeCompress (SCM_OBJ items) (SCM_OBJ weights) same? settings))))
