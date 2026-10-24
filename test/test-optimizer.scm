;;;
;;; sxmml.optimizerの検査
;;;

(use gauche.test)

(test-start "sxmml.optimizer")
(use sxmml.optimizer)
(test-module 'sxmml.optimizer)
(use sxmml)
(use sxmml.form)
(use sxmml.backend.pmd)
(use sxmml.backend.pmd-binary)
(use gauche.uvector)
(use gauche.process)
(use file.util)
(use test.helpers)

;; 命令列の圧縮で使う、既存のリピートのブロックへの組み立て (#69)
(let* ([note (^[v] (make <sxmml-mml-note> :value v :ticks 24))]
       [c (note 0)] [d (note 2)] [e (note 4)]
       [start1 (make <sxmml-mml-repeat-start>)] [start2 (make <sxmml-mml-repeat-start>)]
       [break1 (make <sxmml-mml-repeat-break>)]
       [end1 (make <sxmml-mml-repeat-end> :value 3)] [end2 (make <sxmml-mml-repeat-end> :value 2)]
       ;; [c [d]2 : e]3
       [lis (list start1 c start2 d end2 break1 e end1)]
       [structure (with-module sxmml.optimizer %repeat-structure)]
       [flatten (with-module sxmml.optimizer %repeat-flatten)]
       [expansion (with-module sxmml.optimizer %repeat-expansion)]
       [same-objects? (^[a b] (and (= (length a) (length b)) (every eq? a b)))])
  (test* "repeat structure round-trips" #t
         (same-objects? lis (flatten (structure lis))))
  (test* "repeat expansion" #t
         (same-objects? (list c d d e c d d e c d d) (expansion (structure lis))))
  (test* "a repeat without its end fails" (test-error <error>)
         (structure (list start1 c)))
  (test* "a repeat end without its start fails" (test-error <error>)
         (structure (list c end1)))
  ;; 同値判定。オブジェクトはクラスと全スロット、ブロックは回数・: の有無・本文で比べる
  (let ([same? (with-module sxmml.optimizer %repeat-same?)]
        [block (^[text-items count break?]
                 (car (structure `(,(make <sxmml-mml-repeat-start>)
                                   ,@(if break?
                                       (list (car text-items) (make <sxmml-mml-repeat-break>) (cadr text-items))
                                       text-items)
                                   ,(make <sxmml-mml-repeat-end> :value count)))))])
    (test* "same notes are same" #t (same? (note 0) (note 0)))
    (test* "notes with different ticks are not same" #f
           (same? (note 0) (make <sxmml-mml-note> :value 0 :ticks 12)))
    (test* "a note and a rest are not same" #f
           (same? (note 0) (make <sxmml-mml-rest> :value 0 :ticks 24)))
    (test* "lfo switches on different channels are not same" '(#f #f)
           (list (same? (make <sxmml-mml-lfo-switch> :value 1 :channel 'A)
                        (make <sxmml-mml-lfo-switch> :value 1 :channel 'B))
                 (same? (make <sxmml-mml-lfo-switch> :value 1 :channel #f)
                        (make <sxmml-mml-lfo-switch> :value 1 :channel 'A))))
    (test* "rhythm volumes on different instruments are not same" #f
           (same? (make <sxmml-rhythm-volume> :bass 1) (make <sxmml-rhythm-volume> :snare 1)))
    ;; value を持たない命令はスロットが未束縛のまま比べる
    (test* "unbound slots are compared" '(#t #f)
           (list (same? (make <sxmml-mml-tie>) (make <sxmml-mml-tie>))
                 (same? (make <sxmml-mml-repeat-start>) (make <sxmml-mml-tie>))))
    (test* "blocks compare counts, breaks and bodies" '(#t #f #f #f)
           (list (same? (block (list (note 0) (note 2)) 2 #f) (block (list (note 0) (note 2)) 2 #f))
                 (same? (block (list (note 0) (note 2)) 2 #f) (block (list (note 0) (note 2)) 3 #f))
                 (same? (block (list (note 0) (note 2)) 2 #t) (block (list (note 0) (note 2)) 2 #f))
                 (same? (block (list (note 0) (note 2)) 2 #f) (note 0))))))

(let* ([sxlc-command (make-test-sxlc-command)]
       [workdir (sys-mkdtemp (build-path (temporary-directory) "sxmml-test"))])
  (define (work-file name) (build-path workdir name))
  ;; name.sxl に text を書いて返す
  (define (source-output name text)
    (rlet1 source (work-file #"~|name|.sxl")
      (with-output-to-file source (^[] (display text)))))
  ;; args を付けて sxlc を実行し、終了値と標準エラー出力の行のリストを返す
  (define (sxlc-status . args)
    (let* ([err (work-file "status.err")]
           [p (run-process `(,@sxlc-command ,@args)
                           :output :null :error err :wait #t)])
      (list (sys-wait-exit-status (process-exit-status p)) (file->string-list err))))
  ;; sxlc-status の結果から、終了値と、pattern に一致する標準エラー出力の行の有無を返す
  (define (status-and-message result pattern)
    (list (car result) (boolean (any pattern (cadr result)))))
  ;; forms を最適化の水準 level で PMD 向けに変換し、トラック A の行を返す。
  ;; 最適化はオブジェクトを書き換えるので、水準ごとに読み込み直す
  (define (optimized-line name level forms)
    (let1 source (work-file #"optimize-~|name|.sxl")
      (with-output-to-file source (^[] (for-each write forms)))
      (parameterize ([sxmml-environment (make <sxmml-environment>)]
                     [sxmml-source-file source])
        (pmd-source)
        (slot-set! (sxmml-environment) 'optimize level)
        (load source)
        ((slot-ref (sxmml-environment) 'backend))
        (find #/^A / (string-split (file->string (slot-ref (sxmml-environment) 'output))
                                   "\r\n")))))
  ;; forms を読み込み、トラック track を最適化の水準 level で最適化した列を返す
  (define (optimized-objects track level forms)
    (let1 source (work-file "optimize-objects.sxl")
      (with-output-to-file source (^[] (for-each write forms)))
      (parameterize ([sxmml-environment (make <sxmml-environment>)]
                     [sxmml-source-file source])
        (pmd-source)
        (load source)
        (sxmml-optimize-tree track :optimize level))))
  (unwind-protect
      (begin
        ;; 最適化は、チャンネルの違う LFO の命令を互いに上書きしない (#64)。
        ;; 同じチャンネルの冗長な命令は落とす。チャンネルの省略と A は別に扱う
        (let1 lfo-defs '((define-lfo L1 (1 2 -3 4)) (define-lfo L2 (5 6 7 8)))
          (dolist (case `((#t "Mf Mf{B} c" "*0*B0o4c")
                          (#t "Mf{B} Mf c" "*B0*0o4c")
                          (#t "Mt Mt{B} c" "*1*B1o4c")
                          (#t "M{L1 A} M{L1 B} c" "M1,2,-3,4MB1,2,-3,4o4c")
                          (#t "Mt Mf c" "*0o4c")
                          (#t "Mt Mf{A} c" "*1*0o4c")
                          (:full "Mf Mf{B} c" "*0*B0o4c")
                          (:full "M{L1 A} M{L1 B} c" "M1,2,-3,4MB1,2,-3,4o4c")
                          (:full "Mf c Mf{B} c" "*0o4c*B0c")
                          (:full "M{L1 A} c M{L1 B} c" "M1,2,-3,4o4cMB1,2,-3,4c")
                          (:full "Mt Mf{A} c" "*1*0o4c")
                          (:full "Mt c Mt c" "*1o4cc")
                          (:full "M{L1 A} c M{L2 B} c M{L1 A} c" "M1,2,-3,4o4cMB5,6,7,8cc")
                          (:full "[Mt c Mt{B} c]2" "[*1o4c*B1c]2")
                          (:full "Mt{B} [Mt c Mt{B} c]2" "*B1[*1o4cc]2")
                          (:full "Mt [c Mt c Mf{B} c]2" "*1[o4cc*B0c]2")
                          (:full "M{L1 A} [c M{L1 A} c M{L2 B} c]2" "M1,2,-3,4[o4ccMB5,6,7,8c]2")
                          (:full "Mt [Mt{B} c Mt c Mf c]2" "*1[*B1o4c*1c*0c]2")
                          (:full "Mt c [Mt c Mf c]2" "*1o4c[*1c*0c]2")))
            (test* #"LFO commands \"~(cadr case)\" with optimize ~(car case)"
                   #"A l%24~(caddr case)"
                   (optimized-line "lfo" (car case)
                                   `(,@lfo-defs (mml A ,(cadr case)))))))
        ;; 命令列の反復による圧縮 (#69)。多くは、同じ形を手で書いた MML の :full の出力と比べる
        (let ([nest (^[depth body] (string-append (make-string depth #\[) body
                                                  (string-join (make-list depth "]2") "")))]
              [expansion (with-module sxmml.optimizer %repeat-expansion)]
              [structure (with-module sxmml.optimizer %repeat-structure)]
              [same? (with-module sxmml.optimizer %repeat-same?)])
          (dolist (case '(("[aaaaaaaa]2" "[[o4a]8]2")
                          ("[[aaaa]4[aaaa]4]2" "[[[[o4a]4]4]2]2")
                          ("[ab]2ab" "[o4ab]2ab")
                          ("cdcdcdLcdcdcd" "[o4cd]3L[cd]3")
                          ("c4c4c4c4c8" "[o4c]4c%12")))
            (test* #"\"~(car case)\" with optimize :repeat"
                   #"A l%24~(cadr case)"
                   (optimized-line "repeat" :repeat `((mml A ,(car case))))))
          (dolist (case '(("cdecdecd" "[cd:e]3")
                          ;; 本文が空の既存リピートと、短い列は :full と同じ
                          ("[]2c" "[]2c") ("[c:]2" "[c:]2") ("[:c]2" "[:c]2")
                          ("c" "c") ("cL" "cL") ("L" "L")))
            (test* #"\"~(car case)\" with optimize :repeat is \"~(cadr case)\" with :full"
                   (optimized-line "repeat-hand" :full `((mml A ,(cadr case))))
                   (optimized-line "repeat" :repeat `((mml A ,(car case))))))
          ;; & は音符の直後に置くので、音符と & の間にループの境界を入れない
          (test* "a tie stays right after its note" "A l%24[o4c&]4c"
                 (optimized-line "repeat" :repeat '((mml A "c&c&c&c&c"))))
          (test* "a tie stays right after its note with optimize :repeat-expand" "A l%24[o4c&]4c"
                 (optimized-line "repeat" :repeat-expand '((mml A "[c&]4c"))))
          ;; L を含むブロックは区切りとし、生成するループに入れない。展開もせず、本文だけを
          ;; 圧縮する。MML の解析は反復の中の L を失敗にする (#94) ので、オブジェクトの列を直接渡す
          (let ([objects (^[text]
                           (let loop ([cs (string->list text)] [res '()])
                             (if (null? cs)
                               (reverse res)
                               (case (car cs)
                                 [(#\[) (loop (cdr cs) (cons (make <sxmml-mml-repeat-start>) res))]
                                 [(#\]) (loop (cddr cs) (cons (make <sxmml-mml-repeat-end>
                                                                 :value (digit->integer (cadr cs)))
                                                               res))]
                                 [(#\:) (loop (cdr cs) (cons (make <sxmml-mml-repeat-break>) res))]
                                 [(#\L) (loop (cdr cs) (cons (make <sxmml-mml-loop>) res))]
                                 [else (loop (cdr cs) (cons (make <sxmml-mml-note> :ticks 24
                                                              :value (assv-ref '((#\c . 0) (#\d . 2) (#\e . 4)
                                                                                 (#\f . 5) (#\g . 7) (#\a . 9)
                                                                                 (#\b . 11))
                                                                               (car cs)))
                                                            res))]))))]
                [text (^[lis]
                        (apply string-append
                         (map (^o (cond [(is-a? o <sxmml-mml-repeat-start>) "["]
                                        [(is-a? o <sxmml-mml-repeat-end>) #"]~(slot-ref o 'value)"]
                                        [(is-a? o <sxmml-mml-repeat-break>) ":"]
                                        [(is-a? o <sxmml-mml-loop>) "L"]
                                        [else (string (string-ref "c d ef g a b" (slot-ref o 'value)))]))
                              lis)))]
                [optimize-repeat (with-module sxmml.optimizer %sxmml-optimize-repeat)])
            (dolist (case '(("cdefgab[dLe]2cdefgab" #f "cdefgab[dLe]2cdefgab")
                            ("[cdcdcdLcdcdcd]2" #f "[[cd]3L[cd]3]2")
                            ("[cdLcd]2" #t "[cdLcd]2")
                            ("[c:Ld]1" #t "[c:Ld]1")))
              (test* #"\"~(car case)\" keeps L out of generated loops (expand? ~(cadr case))"
                     (caddr case)
                     (text (optimize-repeat (objects (car case)) (cadr case))))))
          ;; ブロックの 2 個目以降の : も区切りとし、そのブロックは展開しない。MML の解析は
          ;; 2 個目以降の : を失敗にする (#92) ので、オブジェクトの列を直接渡す
          (let* ([d (^[] (make <sxmml-mml-note> :value 2 :ticks 24))]
                 [break (^[] (make <sxmml-mml-repeat-break>))]
                 ;; [c:d:d:d:d]2
                 [lis (list (make <sxmml-mml-repeat-start>) (make <sxmml-mml-note> :value 0 :ticks 24)
                            (break) (d) (break) (d) (break) (d) (break) (d)
                            (make <sxmml-mml-repeat-end> :value 2))]
                 [optimize-repeat (with-module sxmml.optimizer %sxmml-optimize-repeat)])
            (dolist (expand? '(#f #t))
              (test* #"a second break stays out of generated loops (expand? ~expand?)" #t
                     (let1 result (optimize-repeat lis expand?)
                       (and (= (length result) (length lis)) (every eq? result lis))))))
          ;; 圧縮すると入れ子が 33 段になる区間は圧縮しない
          (test* "nesting deeper than 32 is kept"
                 (optimized-line "repeat-hand" :full `((mml A ,(nest 32 "cccccc"))))
                 (optimized-line "repeat" :repeat `((mml A ,(nest 32 "cccccc")))))
          (test* "nesting up to 32 is compressed" #t
                 (boolean (#/\[o4c\]6/ (optimized-line "repeat" :repeat `((mml A ,(nest 31 "cccccc")))))))
          (test* "rhythm L stays outside loops" '(0 2)
                 (let1 lis (optimized-objects 'L :repeat
                                              '((rhythm L "@b8@s8@b8@s8@b8@s8L@b8@s8@b8@s8@b8@s8")))
                   (list (let loop ([rest lis] [depth 0])
                           (cond [(is-a? (car rest) <sxmml-mml-loop>) depth]
                                 [(is-a? (car rest) <sxmml-mml-repeat-start>) (loop (cdr rest) (+ depth 1))]
                                 [(is-a? (car rest) <sxmml-mml-repeat-end>) (loop (cdr rest) (- depth 1))]
                                 [else (loop (cdr rest) depth)]))
                         (count (cut is-a? <> <sxmml-mml-repeat-start>) lis))))
          ;; :repeat-expand は、展開後の長さが上限以下の既存のリピートを展開してから圧縮する
          (dolist (case '(("[aaaaaaaa]2" "[o4a]16")
                          ("[[aaaa]4[aaaa]4]2" "[o4a]64")
                          ("[ab]2ab" "[o4ab]3")
                          ("[cdcdcd]1" "[o4cd]3")))
            (test* #"\"~(car case)\" with optimize :repeat-expand"
                   #"A l%24~(cadr case)"
                   (optimized-line "repeat" :repeat-expand `((mml A ,(car case))))))
          ;; 展開した [cd]64 を含む周期は 128 を超え、2 周期が窓幅 256 に収まらない。
          ;; :repeat-expand は窓幅を広げ、:repeat と同じく外側の反復を見つける
          (test* "a period longer than 128 is found with optimize :repeat-expand"
                 (optimized-line "repeat" :repeat '((mml A "c4d8e16f4g8a16b4[cd]64c4d8e16f4g8a16b4[cd]64")))
                 (optimized-line "repeat" :repeat-expand '((mml A "c4d8e16f4g8a16b4[cd]64c4d8e16f4g8a16b4[cd]64"))))
          (test* "a block of count 1 is kept with optimize :repeat"
                 "A l%24[[o4cd]3]1"
                 (optimized-line "repeat" :repeat '((mml A "[cdcdcd]1"))))
          ;; [cd]3 は展開後の長さ 6 が上限 4 を超えるのでブロックのまま、[cd]2 は展開する
          (test* "a block longer than the limit is kept" '(#t 5)
                 (let* ([o (^[] (make <sxmml-mml-note> :value 0 :ticks 24))]
                        [lis (list (make <sxmml-mml-repeat-start>) (o) (o) (make <sxmml-mml-repeat-end> :value 3)
                                   (make <sxmml-mml-repeat-start>) (o) (o) (make <sxmml-mml-repeat-end> :value 2))]
                        [items ((with-module sxmml.optimizer %repeat-expand-small) (structure lis) 4)])
                   (list ((with-module sxmml.optimizer %repeat-block?) (car items)) (length items))))
          ;; 別の読み込みで作った :full と :repeat の列は、展開すると同値なオブジェクトが同じ順に並ぶ
          (dolist (text '("[aaaaaaaa]2" "[ab]2ab" "cdecdecd" "cdcdcdLcdcdcd"
                          "[cdcdcd]1" "c4c4c4c4c8" "[c:]2" "v10cdv12cdv10cdv12cd" "[c&c&]4"))
            (dolist (level '(:repeat :repeat-expand))
              (test* #"\"~|text|\" keeps its expansion with optimize ~level" #t
                     (let ([a (expansion (structure (optimized-objects 'A :full `((mml A ,text)))))]
                           [b (expansion (structure (optimized-objects 'A level `((mml A ,text)))))])
                       (and (= (length a) (length b)) (every same? a b)))))))
        ;; 最適化で不正なチャンネルの命令が落ちず、-O 0 と同じく失敗する (#64)
        (let1 source (source-output "lfo-invalid-channel"
                                   "(mml A \"Mt{C} Mt c\")")
          (test* "an invalid LFO channel fails with optimize level 1"
                 '(1 #t)
                 (status-and-message (sxlc-status "-O" "1" "-m" "pmd" "-o" (work-file "lfo-invalid.mml") source)
                                     #/invalid lfo channel/)))
      )
    (remove-directory* workdir)))

(let* ([workdir (sys-mkdtemp (build-path (temporary-directory) "sxmml-test"))])
  (define (work-file name) (build-path workdir name))
  (define (source-output path forms)
    (with-output-to-file path (^[] (for-each write forms)))
    path)
  ;; ヘッダの 13 個の位置が指す区間の順。PMD4 の音色表の区間は、続くメタ情報を含む。
  (define binary-sections '(A B C D E F G H I J K R voice))
  ;; モードを選んでソースを読み込み、このプロセスで生成した .m の内容を返す。
  ;; level は最適化の水準。省略時は sxlc の既定の -O 1 と同じ最適化を使う。
  (define (make-binary-data name mode forms :optional (level #t))
    (let1 source (source-output (work-file #"bytes-~|name|.sxl") forms)
      (parameterize ([sxmml-environment (make <sxmml-environment>)]
                     [sxmml-source-file source])
        (mode)
        (slot-set! (sxmml-environment) 'optimize level)
        (load source)
        ((slot-ref (sxmml-environment) 'backend))
        (file->bytevector (slot-ref (sxmml-environment) 'output)))))
  (define (hex-bytes data :optional (start 0) (end (u8vector-length data)))
    (string-join (map (cut format "~2,'0x" <>) (u8vector->list data start end)) " "))
  (define (section-hex data name)
    (let ([position (^i (+ 1 (u8vector-ref data (+ 1 (* i 2)))
                           (ash (u8vector-ref data (+ 2 (* i 2))) 8)))]
          [index (- 13 (length (memq name binary-sections)))])
      (hex-bytes data (position index)
                 (if (< index 12) (position (+ index 1)) (u8vector-length data)))))
  (unwind-protect
      (begin
        ;; 圧縮で生成した : を含むループは、同じ形を手で書いた MML と同じバイト内容になる (#69)
        (test* "pmd-binary の生成したループ"
               (section-hex (make-binary-data "rep-hand" pmd-binary '((mml A "[cd:e]3"))) 'A)
               (section-hex (make-binary-data "rep-gen" pmd-binary '((mml A "cdecdecd")) :repeat) 'A))
      )
    (remove-directory* workdir)))

(test-end :exit-on-failure #t)
