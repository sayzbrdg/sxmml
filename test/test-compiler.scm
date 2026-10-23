;;;
;;; sxmml.compilerの検査
;;;

(use gauche.test)

(test-start "sxmml.compiler")
(use sxmml.compiler)
(test-module 'sxmml.compiler)

(test-end :exit-on-failure #t)
