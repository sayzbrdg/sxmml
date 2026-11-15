;; ビルドした C 拡張を読み込む内部接続モジュール。
(define-module sxmml.native
  (export sxmml-native-repeat-tree-compress sxmml-native-ir-compress
          make-sxmml-native-ir-comparison))
(select-module sxmml.native)
(dynamic-load "sxmml_native")
