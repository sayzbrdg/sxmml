;;;
;;; sxmml.compilerの検査
;;;

(use gauche.test)

(test-start "sxmml.compiler")
(use sxmml.compiler)
(test-module 'sxmml.compiler)

(use parser.peg)
(use sxmml.classes)

(test* "明示的な失敗理由にトラック情報を付ける"
       (test-error <error> "sxmml syntax error: explicit parser failure on G track")
       (sxmml-compile-string! ($raise "explicit parser failure") "x"
                             (make <sxmml-mml-context> :track 'G)))
(test* "通常のPEG失敗の診断を維持する"
       (test-error <error> "sxmml syntax error, near \"x\" on G track")
       (sxmml-compile-string! ($. #\a) "x"
                             (make <sxmml-mml-context> :track 'G)))

(test-end :exit-on-failure #t)
