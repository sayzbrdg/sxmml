;;;
;;; sxmml.compilerの検査
;;;

(use gauche.test)

(test-start "sxmml.compiler")
(use sxmml.compiler)
(test-module 'sxmml.compiler)

(use parser.peg)
(use sxmml.classes)
(use sxmml.environment)

(define (failure-result parser)
  (guard (e [else (list (sxmml-mml-syntax-error? e) (condition-message e)
                        (sxmml-track-context-track e) (condition-has-type? e <parse-error>))])
    (sxmml-compile-string! parser "x" (make <sxmml-mml-context> :track 'G))))
(test* "明示的なPEG失敗をトラック付きのMML構文エラーに変換する"
       '(#t "sxmml syntax error: explicit parser failure on G track" G #f)
       (failure-result ($raise "explicit parser failure")))
(test* "通常のPEG失敗も元のconditionを持ち回らず変換する"
       '(#t "sxmml syntax error, near \"x\" on G track" G #f)
       (failure-result ($. #\a)))
(let1 original (make-condition <error> 'message "parser bug")
  (test* "非PEGの一般エラーは変換しない" #t
         (guard (e [else (eq? e original)])
           (sxmml-compile-string! (^[_] (raise original)) "x"
                                 (make <sxmml-mml-context> :track 'G)))))
(test* "未定義トラックは参照エラーになる"
       '(#t X)
       (parameterize ([sxmml-environment (make <sxmml-environment>)])
         (guard (e [else (list (sxmml-reference-error? e) (sxmml-track-context-track e))])
           (sxmml-compile-track-mml 'X "c"))))
(test* "非文字列引数はトラック付きの値エラーになる"
       '(#t G)
       (guard (e [else (list (sxmml-value-error? e) (sxmml-track-context-track e))])
         (sxmml-compile-string! ($. #\a) 123 (make <sxmml-mml-context> :track 'G))))
(test-end :exit-on-failure #t)
