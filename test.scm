;;;
;;; Test sxmml
;;;

(use gauche.test)

(test-start "sxmml")
(use sxmml)
(test-module 'sxmml)

;; The following is a dummy test code.
;; Replace it for your tests.
(test* "test-sxmml" "sxmml is working"
       (test-sxmml))

;; If you don't want `gosh' to exit with nonzero status even if
;; the test fails, pass #f to :exit-on-failure.
(test-end :exit-on-failure #t)
