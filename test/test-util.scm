;;;
;;; sxmml.utilの検査
;;;

(use gauche.test)

(test-start "sxmml.util")
(use sxmml.util)
(test-module 'sxmml.util)

(test-end :exit-on-failure #t)
