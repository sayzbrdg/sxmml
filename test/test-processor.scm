;;;
;;; sxmml.processorの検査
;;;

(use gauche.test)

(test-start "sxmml.processor")
(use sxmml.processor)
(test-module 'sxmml.processor)
(use sxmml)
(use sxmml.form)
(use sxmml.backend.pmd)
(use sxmml.backend.pmd-source)
(use gauche.process)
(use file.util)
(use test.helpers)

(let* ([workdir (sys-mkdtemp (build-path (temporary-directory) "sxmml-test"))])
  (define (work-file name) (build-path workdir name))
  ;; name.sxl に text を書いて返す
  (define (source-output name text)
    (rlet1 source (work-file #"~|name|.sxl")
      (with-output-to-file source (^[] (display text)))))
  (define (delay-state track)
    (let* ([ir (sxmml-ir-flatten (slot-ref (sxmml-track-get track) 'compiled-data))]
           [notes (filter (^o (and (is-a? o <sxmml-mml-note>)
                                   (not (is-a? o <sxmml-mml-rest>)))) ir)])
      (list (map (cut slot-ref <> 'value) notes)
            (map (cut slot-ref <> 'ticks) notes)
            (slot-ref (sxmml-track-get track) 'current-ticks))))
  (unwind-protect
      (begin
        ;; mml-repeat の回数と、mml-delay・mml-echo の括弧の中の値は式として評価し、
        ;; 実行時に検査する。形は展開時に検査する。誤りにはマクロの開始位置を付ける (#52)
        (let* ([track-lines (^[name forms]
                              (let1 source (work-file #"processor-~|name|.sxl")
                                (with-output-to-file source (^[] (for-each write forms)))
                                (parameterize ([sxmml-environment (make <sxmml-environment>)]
                                               [sxmml-source-file source])
                                  (sxmml-mode-select! 'pmd-source)
                                  (load source)
                                  ((slot-ref (sxmml-mode-get) 'backend)
                                   (make-sxmml-output-environment (sxmml-environment)
                                     :output (path-swap-extension source "mml") :optimize 1 :source source))
                                  (filter #/^[A-L] / (string-split (file->string (path-swap-extension source "mml"))
                                                                   "\r\n")))))]
               [load-error (^[name text]
                             (let1 source (source-output name text)
                               (parameterize ([sxmml-environment (make <sxmml-environment>)]
                                              [sxmml-source-file source])
                                 (sxmml-mode-select! 'pmd-source)
                                 (guard (e [else (cons source e)])
                                   (load source)
                                   (cons source #f)))))]
               [error-report (^[r] (let1 e (cdr r)
                                     (list (sxmml-error? e) (condition-message e)
                                           (sxmml-source-context-location e)
                                           (cond [(sxmml-value-error? e) 'value]
                                                 [(sxmml-form-syntax-error? e) 'form]
                                                 [(sxmml-reference-error? e) 'reference]))))]
          ;; 3 行目の begin の中に form を置いた入力
               [in-begin (^[form] #"; processor\n(begin\n  ~form\n  1)\n")]
               [delay-line "G l%12o4cr%6)%2c%6(%2"]
               [echo-line "G l%12o4c%6)%2g%6(%2d%6)%2c%6)d%6r%6(%3"])
        (test* "先行音に音符がないエコーは位置付きの値エラーになる"
               '(#t #t)
               (let1 r (load-error "echo-no-note" (in-begin "(mml-echo G (\"r\" 2 1) \"c8\")"))
                 (list (sxmml-value-error? (cdr r))
                       (equal? (sxmml-source-context-location (cdr r)) #"~(car r):3"))))

          (dolist [case `(("repeat" ((mml-repeat G 2 ((mml G "c8d8")))) ((mml G "[c8d8]2")))
                          ("repeat-2" ((mml-repeat G 3 ((mml G "c")) ((mml G "d")))) ((mml G "[c:d]3")))
                          ("repeat-3" ((mml-repeat G 2 ((mml G "c")) ((mml G "d")) ((mml G "e"))))
                           ((mml G "[c:d]2e")))
                          ("repeat-tracks" ((mml-repeat (A B) 2 ((mml (A B) "c")))) ((mml (A B) "[c]2")))
                          ("repeat-break-list" ((mml-repeat (A B C) 4 ((mml (A B C) "cdefg")) ((mml (A B C) "ab")) ((B C))))
                           ((mml A "[cdefgab]4") (mml (B C) "[cdefg:ab]4")))
                          ("repeat-break-track" ((mml-repeat (A B) 4 ((mml (A B) "l8 cdefg")) ((mml (A B) "ab")) (B (mml B "a16"))))
                           ((mml A "[l8cdefg ab]4") (mml B "[l8cdefg:ab]4 a16")))
                          ("repeat-variable" ((define %test-count 2) (mml-repeat G %test-count ((mml G "c8d8"))))
                           ((mml G "[c8d8]2")))
                          ("repeat-expression" ((mml-repeat (A B) (+ 1 2) ((mml (A B) "c")) ((mml (A B) "d"))))
                           ((mml (A B) "[c:d]3"))))]
            (test* #"mml-repeat gives the same result as the written repeat: ~(car case)"
                   (track-lines #"~(car case)-hand" (caddr case))
                   (track-lines (car case) (cadr case))))
          (dolist [case `(("delay" ((mml-delay G (6 2 1) "c8r8")) (,delay-line))
                          ("delay-tracks" ((mml-delay (A G) (6 2 1) "c8r8")) ("A l%12o4cr%6)%2c%6(%2" ,delay-line))
                          ("delay-evaluated" ((define %test-ticks 6) (mml-delay G (%test-ticks (+ 1 1) 1) "c8r8"))
                           (,delay-line))
                          ("echo" ((mml-echo G ("g" 2 1) "c8d8r8")) (,echo-line))
                          ("echo-evaluated" ((define %test-note "g") (mml-echo G (%test-note (+ 1 1) 1) "c8d8r8"))
                           (,echo-line)))]
            (test* #"mml-delay and mml-echo take their values: ~(car case)"
                   (caddr case)
                   (track-lines (car case) (cadr case))))
          (test* "mml-repeat without a location takes an evaluated count"
                 2
                 (parameterize ([sxmml-environment (make <sxmml-environment>)])
                   (sxmml-mode-select! 'pmd-source)
                   (eval '(mml-repeat G (+ 1 1) ((mml G "c"))) (interaction-environment))
                   (slot-ref (find (cut is-a? <> <sxmml-mml-repeat-end>)
                                   (sxmml-ir-flatten (slot-ref (sxmml-track-get 'G) 'compiled-data)))
                             'value)))
          (test* "R01 回数式を終了直前に一度評価し本文のスコープを保つ"
                 '((front back count after) (2 2) (((48 . 0)) ((48 . 0))))
                 (let1 source (source-output "repeat-count-once"
                                "(define %repeat-events '())\n(mml-repeat (A B)\n  (begin (set! %repeat-events (append %repeat-events '(count))) %repeat-count)\n  ((define %repeat-count 1)\n   (set! %repeat-events (append %repeat-events '(front)))\n   (mml (A B) \"c8\"))\n  ((set! %repeat-count 2)\n   (set! %repeat-events (append %repeat-events '(back)))\n   (mml (A B) \"d8\"))\n  (() (set! %repeat-events (append %repeat-events '(after)))\n      (set! %repeat-count 3)))\n")
                   (parameterize ([sxmml-environment (make <sxmml-environment>)])
                     (sxmml-mode-select! 'pmd-source)
                     (load source)
                     (list (eval '%repeat-events (interaction-environment))
                           (map (^[track]
                                  (slot-ref (find (cut is-a? <> <sxmml-mml-repeat-end>)
                                                  (sxmml-ir-flatten (slot-ref (sxmml-track-get track) 'compiled-data))) 'value))
                                '(A B))
                           (map (^[track] (slot-ref (sxmml-track-get track) 'current-ticks)) '(A B))))))
          (test* "R02 反復の生成識別子は利用者の束縛に捕捉されない"
                 '((<sxmml-mml-repeat-start>) (<sxmml-mml-note> 48 12) (<sxmml-mml-repeat-end> 2))
                 (let1 source (source-output "repeat-hygiene"
                                "(let ((ctx 7) (count 2)\n      (sxmml-command-repeat-start! #f)\n      (sxmml-command-repeat-break! #f)\n      (sxmml-command-repeat-end! #f)\n      (sxmml-ir-register! #f))\n  (mml-repeat G count ((mml G (if (= ctx 7) \"c8\" \"d8\")))))\n")
                   (parameterize ([sxmml-environment (make <sxmml-environment>)])
                     (sxmml-mode-select! 'pmd-source)
                     (load source)
                     (map (^[ir]
                            (cons (class-name (class-of ir))
                                  (cond [(is-a? ir <sxmml-mml-note>)
                                         (list (slot-ref ir 'value) (slot-ref ir 'ticks))]
                                        [(is-a? ir <sxmml-mml-repeat-end>) (list (slot-ref ir 'value))]
                                        [else '()])))
                          (sxmml-ir-flatten (slot-ref (sxmml-track-get 'G) 'compiled-data))))))
          (let1 r (load-error "repeat-undefined-track" (in-begin "(mml-repeat X 2 ())"))
            (test* "R03 未登録トラックにフォーム位置とトラック情報を付ける"
                   (list #t "undefined track: X" #"~(car r):3" 'X)
                   (let1 e (cdr r)
                     (list (sxmml-reference-error? e) (condition-message e)
                           (sxmml-source-context-location e) (sxmml-track-context-track e)))))
          (test* "D01 delayの3値と本文を一度評価し各トラックで加工する"
                 '(#(1 1 1 1) ((48 48) (12 6) ((24 . 0))) ((60 60 60 60) (24 6 6 6) ((48 . 0))))
                 (parameterize ([sxmml-environment (make <sxmml-environment>)])
                   (sxmml-track-set! 'A :default-length 8 :current-octave 4)
                   (sxmml-track-set! 'B :default-length 4 :current-octave 5)
                   (let* ([calls (make-vector 4 0)]
                          [once (^[index value]
                                  (vector-set! calls index (+ 1 (vector-ref calls index))) value)])
                     (mml-delay (A B) ((once 0 6) (once 1 2) (once 2 1)) (once 3 "cr"))
                     (list calls (delay-state 'A) (delay-state 'B)))))
          (test* "D02 delayの利用者の束縛を保ち生成識別子を捕捉させない"
                 '((48 48) (12 6) ((24 . 0)))
                 (parameterize ([sxmml-environment (make <sxmml-environment>)])
                   (sxmml-track-set! 'G)
                   (let ([text "c8r8"] [ticks 6] [voldown 2] [restdown 1]
                         [write-to-string #f] [list #f] [sxmml-compile-track-mml #f]
                         [sxmml-ir-self-delay! #f] [sxmml-ir-register! #f])
                     (mml-delay G (ticks voldown restdown) text))
                   (delay-state 'G)))
          (let1 r (load-error "delay-shared-value" (in-begin "(mml-delay (A B) ((error <sxmml-value-error> \"sample delay input\") 2 1) \"c8r8\")"))
            (test* "D03 delayの共有値の既知エラーへフォーム位置を付ける"
                   (list #t "sample delay input" #"~(car r):3" 'value)
                   (error-report r)))
          (test* "D04 delayは指定順で先行登録を終えてから後続で失敗する"
                 '(#t "undefined track: X" #t X ((48 48) (12 6) ((24 . 0))))
                 (let1 source (source-output "delay-partial" (in-begin "(mml-delay (A X) (6 2 1) \"c8r8\")"))
                   (parameterize ([sxmml-environment (make <sxmml-environment>)])
                     (sxmml-track-set! 'A)
                     (let1 e (guard (e [else e]) (load source) #f)
                       (list (sxmml-reference-error? e) (condition-message e)
                             (equal? (sxmml-source-context-location e) #"~|source|:3")
                             (sxmml-track-context-track e) (delay-state 'A))))))
          (for-each
           (^[case]
             (test* #"利用者マクロのidentifierを受け取る: ~(car case)"
                    (track-lines #"name-direct-~(car case)" (caddr case))
                    (track-lines #"name-macro-~(car case)"
                                 `((define-syntax user-processor
                                     (syntax-rules () [(_) ,(cadr case)]))
                                   (user-processor)))))
           '((repeat (mml-repeat G 2 ((mml G "c8"))) ((mml G "[c8]2")))
             (echo (mml-echo G ("g" 2 1) "c8d8r8") ((mml-echo G ("g" 2 1) "c8d8r8")))
             (delay (mml-delay G (6 2 1) "c8r8") ((mml-delay G (6 2 1) "c8r8")))))
          (for-each
           (^[case]
             (eval '(define %processor-name-events '()) (interaction-environment))
             (let1 source (source-output #"name-invalid-~(car case)" (in-begin (cadr case)))
               (parameterize ([sxmml-environment (make <sxmml-environment>)])
                 (sxmml-mode-select! 'pmd-source)
                 (let1 e (guard (e [else e]) (load source) #f)
                   (test* #"不正な名前は評価・登録前に拒否する: ~(car case)"
                          (list #t (caddr case) #t '() '() 0)
                          (list (sxmml-form-syntax-error? e) (condition-message e)
                                (equal? (sxmml-source-context-location e) #"~|source|:3")
                                (eval '%processor-name-events (interaction-environment))
                                (slot-ref (sxmml-track-get 'G) 'compiled-data)
                                (length (slot-ref (sxmml-track-get 'G) 'repeat-stack))))))))
           '((delay "(mml-delay (G 0) ((begin (set! %processor-name-events '(value)) 6) 2 1) (begin (set! %processor-name-events '(body)) \"c8r8\"))"
                    "invalid mml-delay track name: 0 (expected a symbol)")
             (echo "(mml-echo (G) (\"g\" 2 1) (begin (set! %processor-name-events '(body)) \"c8\"))"
                   "invalid mml-echo track name: (G) (expected a symbol)")
             (repeat "(mml-repeat 0 2 ((set! %processor-name-events '(body)) (mml G \"c8\")))"
                     "invalid mml-repeat track name: 0 (expected a symbol)")
             (break "(mml-repeat (G) 2 ((set! %processor-name-events '(front)) (mml G \"c8\")) ((set! %processor-name-events '(back))) ((0)))"
                    "invalid mml-repeat track name: 0 (expected a symbol)")))
          (test* "型が正しい未登録名は実行時の参照エラーと先行登録を保つ"
                 '(#t "undefined track: X" ((48 48) (12 6) ((24 . 0))))
                 (let1 source (source-output "name-undefined" (in-begin "(mml-delay (G X) (6 2 1) \"c8r8\")"))
                   (parameterize ([sxmml-environment (make <sxmml-environment>)])
                     (sxmml-mode-select! 'pmd-source)
                     (let1 e (guard (e [else e]) (load source) #f)
                       (list (sxmml-reference-error? e) (condition-message e) (delay-state 'G))))))
          (test* "直接登録された数値名もマクロの構文では拒否する"
                 '(#t "invalid mml-delay track name: 0 (expected a symbol)" ())
                 (let1 source (source-output "name-numeric" (in-begin "(mml-delay 0 (6 2 1) \"c8r8\")"))
                   (parameterize ([sxmml-environment (make <sxmml-environment>)])
                     (hash-table-put! (slot-ref (sxmml-environment) 'track) 0
                                      (make <sxmml-mml-context> :track 0 :part 'G))
                     (let1 e (guard (e [else e]) (load source) #f)
                       (list (sxmml-form-syntax-error? e) (and e (condition-message e))
                             (slot-ref (sxmml-track-get 0) 'compiled-data))))))
          ;; 値の誤りは実行時の値エラーとして通知する
          (dolist [case '(("repeat-zero" "(mml-repeat G 0 ((mml G \"c\")))" "repeat count: 0 (expected a positive exact integer)")
                          ("repeat-negative" "(mml-repeat G -1 ((mml G \"c\")))" "repeat count: -1 (expected a positive exact integer)")
                          ("repeat-fraction" "(mml-repeat G 2.5 ((mml G \"c\")) ((mml G \"d\")))" "repeat count: 2.5 (expected a positive exact integer)")
                          ("repeat-inexact" "(mml-repeat (A B) 2.0 ((mml (A B) \"c\")))" "repeat count: 2.0 (expected a positive exact integer)")
                          ("repeat-string" "(mml-repeat G \"2\" ((mml G \"c\")))" "repeat count: \"2\" (expected a positive exact integer)")
                          ("delay-ticks" "(mml-delay G (0 2 1) \"c8r8\")" "mml-delay value: ticks is 0 (expected a positive integer)")
                          ("delay-inexact-ticks" "(mml-delay (A B) (6.0 2 1) \"c8r8\")" "mml-delay value: ticks is 6.0 (expected a positive integer)")
                          ("delay-voldown" "(mml-delay G (6 1.5 1) \"c8r8\")" "mml-delay value: voldown is 1.5 (expected an integer)")
                          ("delay-restdown" "(mml-delay G (6 2 \"a\") \"c8r8\")" "mml-delay value: restdown is \"a\" (expected an integer)")
                          ("delay-inexact-voldown" "(mml-delay G (6 2.0 1) \"c8r8\")" "mml-delay value: voldown is 2.0 (expected an integer)")
                          ("echo-voldown" "(mml-echo G (\"g\" 1.5 1) \"c8\")" "mml-echo value: voldown is 1.5 (expected an integer)")
                          ("echo-inexact-restdown" "(mml-echo G (\"g\" 2 1.0) \"c8\")" "mml-echo value: restdown is 1.0 (expected an integer)")
                          ("echo-restdown" "(mml-echo G (\"g\" 2 \"a\") \"c8\")" "mml-echo value: restdown is \"a\" (expected an integer)"))]
            (let1 r (load-error #"processor-~(car case)" (in-begin (cadr case)))
              (test* #"a wrong value fails with its location: ~(car case)"
                     (list #t #"invalid ~(caddr case)" #"~(car r):3" 'value)
                     (error-report r))))
          ;; 形の誤りは展開時に構文エラーとして通知する
          (dolist [case '(("repeat-no-body" "(mml-repeat G 2)" "mml-repeat" "(mml-repeat track count (body1 ...) [(body2 ...) [(body3 ...)]])")
                          ("repeat-not-body" "(mml-repeat G 2 1)" "mml-repeat" "(mml-repeat track count (body1 ...) [(body2 ...) [(body3 ...)]])")
                          ("repeat-too-many" "(mml-repeat G 2 () () () ())" "mml-repeat" "(mml-repeat track count (body1 ...) [(body2 ...) [(body3 ...)]])")
                          ("repeat-no-tracks" "(mml-repeat () 2 ((mml G \"c\")))" "mml-repeat" "(mml-repeat track count (body1 ...) [(body2 ...) [(body3 ...)]])")
                          ("repeat-2-no-tracks" "(mml-repeat () 2 ((mml G \"c\")) ((mml G \"d\")))" "mml-repeat" "(mml-repeat track count (body1 ...) [(body2 ...) [(body3 ...)]])")
                          ("repeat-3-no-tracks" "(mml-repeat () 2 ((mml G \"c\")) ((mml G \"d\")) ())" "mml-repeat" "(mml-repeat track count (body1 ...) [(body2 ...) [(body3 ...)]])")
                          ("delay-no-tracks" "(mml-delay () (6 2 1) \"c8r8\")" "mml-delay" "(mml-delay track (ticks voldown restdown) body ...)")
                          ("delay-two-values" "(mml-delay G (6 2) \"c8r8\")" "mml-delay" "(mml-delay track (ticks voldown restdown) body ...)")
                          ("delay-not-list" "(mml-delay G 6 \"c8r8\")" "mml-delay" "(mml-delay track (ticks voldown restdown) body ...)")
                          ("delay-no-values" "(mml-delay G)" "mml-delay" "(mml-delay track (ticks voldown restdown) body ...)")
                          ("echo-two-values" "(mml-echo G (\"g\" 2) \"c8\")" "mml-echo" "(mml-echo track (firstnote voldown restdown) body ...)")
                          ("echo-no-values" "(mml-echo G)" "mml-echo" "(mml-echo track (firstnote voldown restdown) body ...)"))]
            (let1 r (load-error #"processor-~(car case)" (in-begin (cadr case)))
              (test* #"a wrong form fails at expansion with its location: ~(car case)"
                     (list #t #"invalid ~(caddr case) form: expected ~(cadddr case)" #"~(car r):3" 'form)
                     (error-report r))))
          (let1 r (load-error "processor-undefined-count" (in-begin "(mml-repeat G %test-undefined-count ((mml G \"c\")))"))
            (test* "回数式の未定義変数をGaucheのエラーとして通す"
                   '(#t #f #f)
                   (let1 e (cdr r)
                     (list (condition-has-type? e <unbound-variable-error>)
                             (sxmml-error? e) (sxmml-source-context? e)))))
          (let1 r (load-error "processor-delay-track" (in-begin "(mml-delay X (6 2 1) \"c8r8\")"))
            (test* "an undefined track of mml-delay fails with its location"
                   (list #t #"undefined track: X" #"~(car r):3" 'reference)
                   (error-report r)))
          )
      )
    (remove-directory* workdir)))

(test-end :exit-on-failure #t)
