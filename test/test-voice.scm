;;;
;;; sxmml.voiceの検査
;;;

(use gauche.test)

(test-start "sxmml.voice")
(use sxmml.voice)
(test-module 'sxmml.voice)
(use sxmml)
(use sxmml.form)
(use sxmml.backend.pmd)
(use gauche.process)
(use file.util)
(use test.helpers)

;; 音色の登録 (#27)
;; sxmml-opn-voice-set! は実行時に検査して登録し、define-opn-voice はその呼び出しに展開する
(let* ([op0 '(0 0 0 0 0 0 0 0 0)]
       [ops (make-list 4 op0)]
       [voice-value (^[env id] (slot-ref (hash-table-get (slot-ref env 'voice) id) 'value))])
  (define (with-new-environment thunk)
    (parameterize ([sxmml-environment (make <sxmml-environment>)])
      (thunk)))
  (test* "sxmml-opn-voice-set! registers a voice"
         `((1 2) ,@(make-list 4 (make-list 12 0)))
         (with-new-environment
          (^[] (apply sxmml-opn-voice-set! 'v '(1 2) ops)
               (voice-value (sxmml-environment) 'v))))
  ;; 10 値は dt2 と seg、11 値は dt2 を 0 として補う
  (let ([op10 '(1 2 3 4 5 6 7 8 9 10)]
        [op11 '(1 2 3 4 5 6 7 8 9 11 12)]
        [expected '((0 0) (1 2 3 4 5 6 7 8 9 0 0 10) (1 2 3 4 5 6 7 8 9 0 11 12)
                    (0 0 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0 0 0 0 0))])
    (test* "operators of 10 and 11 values are padded at dt2 and seg"
           expected
           (with-new-environment
            (^[] (sxmml-opn-voice-set! 'v '(0 0) op10 op11 op0 op0)
                 (voice-value (sxmml-environment) 'v))))
    (test* "define-opn-voice pads operators of 10 and 11 values at dt2 and seg"
           expected
           (with-new-environment
            (^[] (eval `(define-opn-voice v (0 0) ,op10 ,op11 ,op0 ,op0) (interaction-environment))
                 (voice-value (sxmml-environment) 'v)))))
  (dolist [case `(("a non-symbol voice id" "invalid voice id: 0 (expected a symbol)" (0 (0 0) ,@ops))
                  ("an invalid al/fb" "invalid voice @v al/fb: (0) (expected a list of 2 values)" (v (0) ,@ops))
                  ("an invalid operator" "invalid voice @v operator(2): (0 0 0) (expected a list of 9 to 12 values)"
                   (v (0 0) ,op0 (0 0 0) ,op0 ,op0))
                  ("a negative value" "invalid voice @v operator(2): -1 (expected a nonnegative exact integer)"
                   (v (0 0) ,op0 (-1 0 0 0 0 0 0 0 0) ,op0 ,op0))
                  ("an inexact value" "invalid voice @v operator(2): 1.5 (expected a nonnegative exact integer)"
                   (v (0 0) ,op0 (1.5 0 0 0 0 0 0 0 0) ,op0 ,op0))
                  ("a non-number value" "invalid voice @v operator(2): x (expected a nonnegative exact integer)"
                   (v (0 0) ,op0 (x 0 0 0 0 0 0 0 0) ,op0 ,op0))
                  ("a negative al/fb" "invalid voice @v al/fb: -1 (expected a nonnegative exact integer)" (v (0 -1) ,@ops)))]
    (test* #"~(car case) fails" (test-error <sxmml-value-error> (cadr case))
           (with-new-environment (^[] (apply sxmml-opn-voice-set! (caddr case))))))
  ;; 同じ id の定義の判定は実行時に行う。前の環境で展開した定義も、実行した環境に登録する
  (let ([env1 (make <sxmml-environment>)]
        [env2 (make <sxmml-environment>)])
    (parameterize ([sxmml-environment env1])
      (eval `(define-opn-voice V (1 0) ,@ops) (interaction-environment))
      (eval `(define (%test-define-voice-v) (define-opn-voice V (2 0) ,@ops)) (interaction-environment)))
    (test* "a voice is registered when the definition runs"
           '((1 0) (2 0))
           (parameterize ([sxmml-environment env2])
             ((eval '%test-define-voice-v (interaction-environment)))
             (list (car (voice-value env1 'V)) (car (voice-value env2 'V))))))
  (test* "the later definition of the same id in begin replaces the former"
         '(2 0)
         (with-new-environment
          (^[] (eval `(begin (define-opn-voice W (1 0) ,@ops) (define-opn-voice W (2 0) ,@ops))
                     (interaction-environment))
               (car (voice-value (sxmml-environment) 'W))))))

(let* ([workdir (sys-mkdtemp (build-path (temporary-directory) "sxmml-test"))])
  (define (work-file name) (build-path workdir name))
  ;; name.sxl に text を書いて返す
  (define (source-output name text)
    (rlet1 source (work-file #"~|name|.sxl")
      (with-output-to-file source (^[] (display text)))))
  (unwind-protect
      (begin
        ;; define-opn-voice の枠の中の要素は式として評価する。定義の誤りは、展開時でも
        ;; 実行時でも、フォームの開始位置を独立したconditionとして付ける (#28)
        (let* ([op0 "(0 0 0 0 0 0 0 0 0)"]
               ;; 3 行目の begin の中の定義。op2 と al/fb を差し替えられる
               [voice-text (^[alfb op2] #"; voice\n(begin\n  (define-opn-voice V1 ~alfb\n    ~op0 ~op2 ~op0 ~op0)\n  (define %test-after-voice 1))\n")]
               [load-result (^[name text]
                              (let1 source (source-output name text)
                                (parameterize ([sxmml-environment (make <sxmml-environment>)]
                                               [sxmml-source-file source])
                                  (guard (e [else (cons source e)])
                                    (load source)
                                    (cons source (hash-table-get (slot-ref (sxmml-environment) 'voice) 'V1 #f))))))]
               [error-report (^[r] (let1 e (cdr r)
                                     (list (sxmml-error? e) (condition-message e)
                                           (sxmml-source-context-location e))))])
          (let1 r (load-result "voice-evaluated"
                               #"(define %test-al 3)\n(define-opn-voice V1 (%test-al 0) (1 (+ 1 2) 0 0 0 0 0 0 0) ~op0 ~op0 ~op0)\n")
            (test* "elements of define-opn-voice are evaluated"
                   '((3 0) (1 3 0 0 0 0 0 0 0 0 0 0))
                   (let1 v (slot-ref (cdr r) 'value) (list (car v) (cadr v)))))
          (let1 r (load-result "voice-wrong-operator" (voice-text "(0 0)" "(0 0 0)"))
            (test* "a wrong operator in begin fails with its location"
                   (list #t #"invalid voice @V1 operator(2): (0 0 0) (expected a list of 9 to 12 values)" #"~(car r):3" #t)
                   (append (error-report r) (list (sxmml-form-syntax-error? (cdr r))))))
          ;; 要素の数の誤りは展開時に構文エラーとして通知する
          (dolist [case `(("voice-wrong-alfb" "(0)" ,op0 "al/fb: (0) (expected a list of 2 values)")
                          ("voice-8-values" "(0 0)" "(0 0 0 0 0 0 0 0)" "operator(2): (0 0 0 0 0 0 0 0) (expected a list of 9 to 12 values)")
                          ("voice-13-values" "(0 0)" "(0 0 0 0 0 0 0 0 0 0 0 0 0)"
                           "operator(2): (0 0 0 0 0 0 0 0 0 0 0 0 0) (expected a list of 9 to 12 values)"))]
            (let1 r (load-result (car case) (voice-text (cadr case) (caddr case)))
              (test* #"a wrong number of values fails at expansion with its location: ~(car case)"
                     (list #t #"invalid voice @V1 ~(cadddr case)" #"~(car r):3" #t)
                     (append (error-report r) (list (sxmml-form-syntax-error? (cdr r)))))))
          (dolist [case `(("voice-three-operators" ,#"; voice\n(begin\n  (define-opn-voice V1 (0 0)\n    ~op0 ~op0 ~op0)\n  1)\n")
                          ("voice-variable-operator" ,(voice-text "(0 0)" "op")))]
            (let1 r (load-result (car case) (cadr case))
              (test* #"a wrong define-opn-voice form fails with its location: ~(car case)"
                     (list #t #"invalid define-opn-voice form: expected (define-opn-voice id (al fb) op1 op2 op3 op4)" #"~(car r):3")
                     (error-report r))))
          (let1 r (load-result "voice-wrong-value" (voice-text "(0 0)" "(-1 0 0 0 0 0 0 0 0)"))
            (test* "a wrong voice value fails with its location"
                   (list #t #"invalid voice @V1 operator(2): -1 (expected a nonnegative exact integer)" #"~(car r):3"
                          #t)
                   (append (error-report r)
                           (list (sxmml-value-error? (cdr r))))))
          (let1 r (load-result "voice-undefined-element" (voice-text "(0 0)" "(%test-undefined-element 0 0 0 0 0 0 0 0)"))
            (test* "音色要素の未定義変数をGaucheのエラーとして通す"
                   '(#t #f #f)
                   (let1 e (cdr r)
                     (list (condition-has-type? e <unbound-variable-error>)
                             (sxmml-error? e) (sxmml-source-context? e)))))
          )
      )
    (remove-directory* workdir)))

(test-end :exit-on-failure #t)
