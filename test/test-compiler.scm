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
       '(#t "explicit parser failure" G #f)
       (failure-result ($raise "explicit parser failure")))
(test* "通常のPEG失敗も元のconditionを持ち回らず変換する"
       '(#t "invalid MML syntax near \"x\"" G #f)
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
(test* "G01 通常MMLの空IRと解析の継続"
       '(() ((<sxmml-mml-note> 60 12) (<sxmml-mml-note> 62 6)) ((18 . 0)) 16)
       (let* ([ctx (make <sxmml-mml-context>)]
              [empty (sxmml-compile-mml! "l8o5" ctx)]
              [ir (sxmml-compile-mml! "cl16d" ctx)])
         (list empty
               (map (^[o] (list (class-name (class-of o)) (slot-ref o 'value) (slot-ref o 'ticks))) ir)
               (slot-ref ctx 'current-ticks) (slot-ref ctx 'default-length))))
(test* "G02 リズムMMLの解析の継続"
       '((<sxmml-rhythm-emit> <sxmml-mml-rest>) #t 12 #f 6 ((18 . 0)) 16)
       (let* ([ctx (make <sxmml-mml-context>)] [ir (sxmml-compile-rhythm! "l8@bl16r" ctx)])
         (list (map (compose class-name class-of) ir)
               (slot-ref (car ir) 'bass) (slot-ref (car ir) 'ticks)
               (slot-ref (cadr ir) 'value) (slot-ref (cadr ir) 'ticks)
               (slot-ref ctx 'current-ticks) (slot-ref ctx 'default-length))))
(test-end :exit-on-failure #t)
