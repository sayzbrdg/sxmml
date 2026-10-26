;;;
;;; sxmml.mmlの検査
;;;

(use gauche.test)

(test-start "sxmml.mml")
(use sxmml.mml)
(test-module 'sxmml.mml)
(use sxmml)
(use sxmml.form)
(use sxmml.backend.pmd)

(let ()
  (define (compile rhythm? text)
    (parameterize ([sxmml-environment (make <sxmml-environment>)])
      (define-track G)
      (define-track L)
      (if rhythm? (rhythm L text) (mml G text))))
  (test* "通常MMLの未知のxを報告する"
         (test-error <sxmml-mml-syntax-error> "unknown MML command: \"x\"")
         (compile #f "c8x"))
  (test* "リズムMMLの未知のxを報告する"
         (test-error <sxmml-mml-syntax-error> "unknown MML command: \"x\"")
         (compile #t "@b8x"))
  (test* "登録済みリズム命令のtickエラーを維持する"
         (test-error <sxmml-mml-syntax-error> "invalid tick length: 0 (expected a positive exact integer)")
         (compile #t "@b%0")))

;; 反復回数は正の正確な整数。1回の反復の意味と、脱出位置の状態も確認する (#106)
(let ()
  (define (compile rhythm? text)
    (parameterize ([sxmml-environment (make <sxmml-environment>)])
      (define-track G)
      (define-track L)
      (if rhythm? (rhythm L text) (mml G text))
      (let* ([track (if rhythm? 'L 'G)]
             [objects (sxmml-optimize-tree track :optimize #f)]
             [env (sxmml-track-get track)])
        (list objects (map (cut slot-ref env <>)
                           '(total-ticks default-length current-octave current-transpose))))))
  (dolist [case '((#f "[ab]0") (#f "[a:b]0") (#f "[ab]{0}") (#f "[a:b]000")
                  (#t "[@b8@s8]0") (#t "[@b8:@s8]0")
                  (#t "[@b8@s8]{0}") (#t "[@b8:@s8]000"))]
    (test* #"反復回数0を回数の誤りとして拒否する: ~(cadr case)"
           (test-error <sxmml-mml-syntax-error> "invalid repeat count: 0 (expected a positive exact integer)")
           (begin (compile (car case) (cadr case)) 'accepted)))
  (dolist [rhythm? '(#f #t)]
    (test* #"開始命令のない反復終端を拒否する: リズム ~rhythm?"
           (test-error <sxmml-mml-syntax-error> "repeat end appeared before repeat start")
           (compile rhythm? "]1")))
  (test* "tickの誤りは従来の文言で通知する"
         (test-error <sxmml-mml-syntax-error> "invalid tick length: 0 (expected a positive exact integer)")
         (compile #f "c%0"))
  ;; 音符の値は octave * 12 + 音名 + 移調。a=57、b=59、o5K$2 の a=71・c=62。
  ;; 通常の4分音符は24 tick、l8では12 tick。脱出ありの2回は a b a の72 tick。
  (dolist [case '((#f "[ab]1" (48 4 4 0) (1) ((57 24) (59 24)))
                  (#f "[a:b]1" (24 4 4 0) (1) ((57 24)))
                  (#f "[ab]001" (48 4 4 0) (1) ((57 24) (59 24)))
                  (#f "[a:b]{1}" (24 4 4 0) (1) ((57 24)))
                  (#f "[l8o5K$2a:l16o3K$0b]1c" (24 8 5 2) (1) ((71 12) (62 12)))
                  (#f "[a:[b:c]1d]1e" (48 4 4 0) (1 1) ((57 24) (52 24)))
                  (#f "[a:b]2" (72 4 4 0) (2) ((57 24) (59 24) (57 24)))
                  (#t "[@b8@s8]1" (24 4 4 0) (1) (((bass) 12) ((snare) 12)))
                  (#t "[@b8:@s8]1" (12 4 4 0) (1) (((bass) 12)))
                  (#t "[l8@b:l16@s]1@h" (24 8 4 0) (1) (((bass) 12) ((hihat) 12))))]
    (let* ([rhythm? (car case)]
           [result (compile rhythm? (cadr case))]
           [objects (car result)]
           [expanded ((with-module sxmml.optimizer %repeat-expansion)
                      ((with-module sxmml.optimizer %repeat-structure) objects))])
      (test* #"正の回数の意味・集計・状態を保ち、反復を保持する: ~(cadr case)"
             (cddr case)
             (list (cadr result)
                   (filter-map (^o (and (is-a? o <sxmml-mml-repeat-end>) (slot-ref o 'value))) objects)
                   (map (^o (list (if rhythm?
                                    (filter (^s (slot-ref o s)) '(bass snare tom cymbal hihat rimshot))
                                    (slot-ref o 'value))
                                  (slot-ref o 'ticks)))
                        (filter (cut is-a? <> <sxmml-mml-note>) expanded)))))))

;; 反復の : は一つの反復に一つだけ (#92)
(let1 compile (^[text] (parameterize ([sxmml-environment (make <sxmml-environment>)])
                         (define-track G)
                         (mml G text)
                         'compiled))
  (test* "a second break in a repeat fails"
         (test-error <sxmml-mml-syntax-error> "break command can be used only once in a repeat")
         (compile "[c:d:e]2"))
  (test* "breaks in nested repeats are accepted" 'compiled
         (compile "[c:[d:e]2f]2"))
  (test* "a break outside of repeats fails"
         (test-error <sxmml-mml-syntax-error> "break command cannot be used outside of repeats")
         (compile ":c")))

;; L は反復の外にだけ書ける。反復は同じ位置を何度も通るので、戻り先を一つに決められない (#94)
(let-syntax ([compiled (syntax-rules ()
                         [(_ form ...)
                          (parameterize ([sxmml-environment (make <sxmml-environment>)])
                            (define-track G)
                            (define-track L)
                            form ...
                            'compiled)])])
  (let1 loop-error (test-error <sxmml-mml-syntax-error> "loop command cannot be used inside repeats")
    (test* "a loop in a repeat fails" loop-error (compiled (mml G "c[dLe]2")))
    (test* "a loop in a nested repeat fails" loop-error (compiled (mml G "[c[dLe]2f]2")))
    (test* "a loop after a break fails" loop-error (compiled (mml G "[c:dL]2")))
    (test* "a loop in a repeat over calls fails" loop-error (compiled (mml G "[c") (mml G "Ld]2")))
    (test* "a loop in the body of mml-repeat fails" loop-error (compiled (mml-repeat G 2 ((mml G "cLd")))))
    (test* "a loop in a rhythm repeat fails" loop-error (compiled (rhythm L "@b8[@s8L@b8]2"))))
  (test* "loops outside of repeats are accepted" '(compiled compiled)
         (list (compiled (mml G "[cd]2Le[fg]2"))
               (compiled (rhythm L "[@b8]2L[@s8]2"))))
  (test* "同じ文字列の2個目のLを拒否する"
         (test-error <sxmml-mml-syntax-error> "loop command can be used only once in a track")
         (compiled (mml G "c8Ld8Le8")))
  (test* "登録をまたぐ2個目のLを拒否する"
         (test-error <sxmml-mml-syntax-error> "loop command can be used only once in a track")
         (compiled (rhythm L "@b8L@s8") (rhythm L "L@h8")))
  (test* "別トラックではそれぞれLを使用できる" 'compiled
         (compiled (mml G "c8Ld8") (rhythm L "@b8L@s8"))))

(let ()
  (define (compile rhythm? text)
    (parameterize ([sxmml-environment (make <sxmml-environment>)])
      (pmd-source)
      (if rhythm? (rhythm L text) (mml G text))
      (sxmml-optimize-tree (if rhythm? 'L 'G) :optimize #f)
      (slot-ref (sxmml-track-get (if rhythm? 'L 'G)) 'total-ticks)))
  (dolist [case '((#f "c0") (#f "r0") (#f "l0") (#t "@b0") (#t "l0"))]
    (test* #"音長0を計算・登録前に拒否する: ~(cadr case)"
           (test-error <sxmml-mml-syntax-error>
                       "invalid note length: 0 (expected an integer >= 1)")
           (compile (car case) (cadr case))))
  (test* "正の音長と音長省略を維持する" '(96 96 192 1 24)
         (map (cut compile #f <>) '("c1" "r1" "l1cr" "c%1" "c"))))

(test-end :exit-on-failure #t)
