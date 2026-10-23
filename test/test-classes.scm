;;;
;;; sxmml.classesの検査
;;;

(use gauche.test)

(test-start "sxmml.classes")
(use sxmml.classes)
(test-module 'sxmml.classes)

(test-end :exit-on-failure #t)
