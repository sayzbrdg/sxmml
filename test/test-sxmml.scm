;;;
;;; sxmmlの検査
;;;

(use gauche.test)

(test-start "sxmml")
(use sxmml)
(test-module 'sxmml)

(test-end :exit-on-failure #t)
