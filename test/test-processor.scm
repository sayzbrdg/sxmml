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
(use gauche.process)
(use file.util)
(use test.helpers)

(let* ([workdir (sys-mkdtemp (build-path (temporary-directory) "sxmml-test"))])
  (define (work-file name) (build-path workdir name))
  ;; name.sxl に text を書いて返す
  (define (source-output name text)
    (rlet1 source (work-file #"~|name|.sxl")
      (with-output-to-file source (^[] (display text)))))
  (unwind-protect
      (begin
        ;; mml-repeat の回数と、mml-delay・mml-echo の括弧の中の値は式として評価し、
        ;; 実行時に検査する。形は展開時に検査する。誤りにはマクロの開始位置を付ける (#52)
        (let* ([track-lines (^[name forms]
                              (let1 source (work-file #"processor-~|name|.sxl")
                                (with-output-to-file source (^[] (for-each write forms)))
                                (parameterize ([sxmml-environment (make <sxmml-environment>)]
                                               [sxmml-source-file source])
                                  (pmd-source)
                                  (load source)
                                  ((slot-ref (sxmml-environment) 'backend))
                                  (filter #/^[A-L] / (string-split (file->string (slot-ref (sxmml-environment) 'output))
                                                                   "\r\n")))))]
               [load-error (^[name text]
                             (let1 source (source-output name text)
                               (parameterize ([sxmml-environment (make <sxmml-environment>)]
                                              [sxmml-source-file source])
                                 (pmd-source)
                                 (guard (e [else (cons source e)])
                                   (load source)
                                   (cons source #f)))))]
               [error-report (^[r] (let1 e (cdr r)
                                     (list (sxmml-form-error? e) (condition-message e)
                                           (sxmml-form-error-location e)
                                           (and-let1 cause (sxmml-form-error-cause e)
                                             (condition-message cause)))))]
               ;; 3 行目の begin の中に form を置いた入力
               [in-begin (^[form] #"; processor\n(begin\n  ~form\n  1)\n")]
               [delay-line "G l%12o4cr%6)%2c%6(%2"]
               [echo-line "G l%12o4c%6)%2g%6(%2d%6)%2c%6)d%6r%6(%3"])
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
                   (pmd-source)
                   (eval '(mml-repeat G (+ 1 1) ((mml G "c"))) (interaction-environment))
                   (slot-ref (find (cut is-a? <> <sxmml-mml-repeat-end>)
                                   (sxmml-optimize-tree 'G :optimize #f))
                             'value)))
          ;; 値の誤りは実行時に検査するので、元のエラーを cause に持つ
          (dolist [case '(("repeat-zero" "(mml-repeat G 0 ((mml G \"c\")))" "mml-repeat value: count is 0 (expected a positive integer)")
                          ("repeat-negative" "(mml-repeat G -1 ((mml G \"c\")))" "mml-repeat value: count is -1 (expected a positive integer)")
                          ("repeat-fraction" "(mml-repeat G 2.5 ((mml G \"c\")) ((mml G \"d\")))" "mml-repeat value: count is 2.5 (expected a positive integer)")
                          ("repeat-inexact" "(mml-repeat (A B) 2.0 ((mml (A B) \"c\")))" "mml-repeat value: count is 2.0 (expected a positive integer)")
                          ("repeat-string" "(mml-repeat G \"2\" ((mml G \"c\")))" "mml-repeat value: count is \"2\" (expected a positive integer)")
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
                     (list #t #"~(car r):3: invalid ~(caddr case)" #"~(car r):3" #"invalid ~(caddr case)")
                     (error-report r))))
          ;; 形の誤りは展開時に検査するので、cause を持たない
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
                     (list #t #"~(car r):3: invalid ~(caddr case) form: expected ~(cadddr case)" #"~(car r):3" #f)
                     (error-report r))))
          (let1 r (load-error "processor-undefined-count" (in-begin "(mml-repeat G %test-undefined-count ((mml G \"c\")))"))
            (test* "an undefined variable in the count fails with its location"
                   (list #t #t #"~(car r):3" #t)
                   (let1 e (cdr r)
                     (list (sxmml-form-error? e)
                           (boolean (#/^\S+:3: / (condition-message e)))
                           (sxmml-form-error-location e)
                           (is-a? (sxmml-form-error-cause e) <unbound-variable-error>)))))
          (let1 r (load-error "processor-delay-track" (in-begin "(mml-delay X (6 2 1) \"c8r8\")"))
            (test* "an undefined track of mml-delay fails with its location"
                   (list #t #"~(car r):3: no defined track:  X" #"~(car r):3" "no defined track:  X")
                   (error-report r)))
          )
      )
    (remove-directory* workdir)))

(test-end :exit-on-failure #t)
