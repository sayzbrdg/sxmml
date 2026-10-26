;;;
;;; sxmml.backend.pmd-binaryの検査
;;;

(use gauche.test)

(test-start "sxmml.backend.pmd-binary")
(use sxmml.backend.pmd-binary)
(test-module 'sxmml.backend.pmd-binary)
(use sxmml)
(use sxmml.form)
(use sxmml.backend.pmd)
(use gauche.uvector)
(use gauche.process)
(use file.util)
(use test.helpers)

(let* ([sxlc-command (make-test-sxlc-command)]
       [workdir (sys-mkdtemp (build-path (temporary-directory) "sxmml-test"))])
  (define (work-file name) (build-path workdir name))
  (define (source-output path forms)
    (with-output-to-file path (^[] (for-each write forms)))
    path)
  (define (run-sxlc source . options)
    (let* ([status 0]
           [lines (process-output->string-list
                   `(,@sxlc-command ,@options ,source)
                   :error :merge
                   :on-abnormal-exit (^p (set! status (process-exit-status p))))])
      (list (sys-wait-exit-status status) lines)))
  ;; AL と FB の合成値で、出力された音色を識別する。
  (define (voice-form id marker)
    `(define-opn-voice ,id (,(modulo marker 8) ,(quotient marker 8))
       ,@(make-list 4 '(0 0 0 0 0 0 0 0 0))))
  ;; 先頭の 0 バイトに続くヘッダの 24・25 バイト目が音色表の位置。
  ;; 各音色は番号 1 バイト、音色データ 25 バイト（最後が AL/FB）。
  (define (voice-records path pmd3?)
    (let* ([data (file->bytevector path)]
           [start (+ 1 (u8vector-ref data 25) (ash (u8vector-ref data 26) 8))])
      (let loop ([offset start] [records '()])
        (if (if pmd3?
              (= offset (u8vector-length data))
              (and (= (u8vector-ref data offset) 0)
                   (= (u8vector-ref data (+ offset 1)) #xff)))
          (reverse records)
          (loop (+ offset 26)
                (cons (list (u8vector-ref data offset) (u8vector-ref data (+ offset 25)))
                      records))))))
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
  (define zero-voice
    `(define-opn-voice 0 (0 0) ,@(make-list 4 '(0 0 0 0 0 0 0 0 0))))
  ;; パーサーを通さず、ループ開始が2個のIRをバックエンドへ渡す。
  (define (multiple-loop-output track)
    (define (event instrument)
      (if (eq? track 'L)
        (make <sxmml-rhythm-emit> :value #t :ticks 12 instrument #t)
        (make <sxmml-mml-rest> :ticks 12)))
    (parameterize ([sxmml-environment (make <sxmml-environment>)]
                   [sxmml-source-file (work-file "direct-loop.sxl")])
      (pmd-binary)
      (slot-set! (sxmml-environment) 'output (work-file #"direct-loop-~|track|.m"))
      (slot-set! (sxmml-environment) 'optimize #f)
      (sxmml-ir-register! (list (event :bass) (make <sxmml-mml-loop>)
                                (event :snare) (make <sxmml-mml-loop>)
                                (event :hihat)) track)
      (sxmml-pmd-binary-output)))
  (define (raw-voice-data name alfb op :optional (used? #t))
    (make-binary-data name pmd-binary
                      `((hash-table-put! (slot-ref (sxmml-environment) 'voice) 'tone
                                         (make <sxmml-opn-voice> :id 'tone
                                               :value ',(cons alfb (make-list 4 op))))
                        (mml A ,(if used? "@{tone}o4c%12" "o4c%12"))) #f))
  (unwind-protect
      (begin
        (dolist [case '(("SSG" G 16 "invalid SSG volume: 16 (expected an integer from 0 to 15)")
                        ("PCM" J 256 "invalid PCM volume: 256 (expected an integer from 0 to 255)"))]
          (test* #"同じ命令コードでも~(car case)の音量範囲を示す"
                 (test-error <sxmml-value-error> (cadddr case))
                 (make-binary-data #"typed-~(car case)-volume" pmd-binary
                                   `((mml ,(cadr case) ,#"v~(caddr case)c8")))))
        (dolist [case '(("timer" <sxmml-mml-tempo-absolute> 251
                         "invalid Timer-B: 251 (expected an integer from 0 to 250)")
                        ("detune" <sxmml-mml-detune> 32768
                         "invalid detune: 32768 (expected an integer from -32768 to 32767)"))]
          (test* #"符号化の項目と単位を示す: ~(car case)"
                 (test-error <sxmml-value-error> (cadddr case))
                 (make-binary-data #"typed-~(car case)" pmd-binary
                                   `((sxmml-ir-register!
                                      (list (make ,(cadr case) :value ,(caddr case))
                                            (make <sxmml-mml-note> :value 48 :ticks 12)) 'A)) #f)))

        (test* "BPM換算後の出力上限超過は計算結果とBPMを示す"
               (test-error <sxmml-value-error>
                           "invalid calculated Timer-B: 251 (from BPM 2000; expected an integer from 0 to 250)")
               (make-binary-data "calculated-timer" pmd-binary
                                 '((sxmml-resolution-set! 24)
                                   (sxmml-ir-register!
                                    (list (make <sxmml-mml-tempo> :value 2000)
                                          (make <sxmml-mml-note> :value 48 :ticks 12)) 'A)) #f))
        (dolist [case '(("FM" A 127 "7f") ("SSG" G 15 "0f") ("PCM" J 255 "ff"))]
          (let ([kind (car case)] [track (cadr case)] [limit (caddr case)])
            (define (relative-data name value)
              (make-binary-data name pmd-binary
                                `((sxmml-ir-register!
                                   (list (make <sxmml-mml-volume-change> :value ,value)
                                         (make <sxmml-mml-note> :value 48 :ticks 12)) ',track)) #f))
            (test* #"~|kind|相対音量の範囲外は符号付きの指定値と範囲を示す"
                   (test-error <sxmml-value-error>
                               (format "invalid ~a relative volume: ~a (expected an integer from ~a to ~a)"
                                       kind (- (+ limit 1)) (- limit) limit))
                   (relative-data #"relative-~|kind|-bad" (- (+ limit 1))))
            (test* #"~|kind|相対音量の負側境界の符号化を維持する"
                   (format "e2 ~a 30 0c 80" (cadddr case))
                   (section-hex (relative-data #"relative-~|kind|-boundary" (- limit)) track))))

        (let ([upper '(31 31 31 15 15 127 3 15 7 3 15 1)]
              [ranges '((AL 0 7) (FB 1 7) (AR 0 31) (DR 1 31) (SR 2 31)
                        (RR 3 15) (SL 4 15) (TL 5 127) (KS 6 3) (ML 7 15)
                        (DT 8 7) (AMS 11 1))])
          (dolist [r ranges]
            (let* ([field (car r)] [index (cadr r)] [high (caddr r)]
                   [common? (memq field '(AL FB))])
              (test* #"手作り音色の~|field|の上限超過を出力時に拒否する"
                     (test-error <sxmml-value-error>
                                 (format "invalid voice @tone~a ~a: ~a (expected an integer from 0 to ~a)"
                                         (if common? "" " operator(1)") field (+ high 1) high))
                     (raw-voice-data #"raw-upper-~field"
                                     (map (^[i] (if (and common? (= i index)) (+ high 1) 0)) (iota 2))
                                     (map (^[i] (if (and (not common?) (= i index)) (+ high 1) 0))
                                          (iota 12))))))
          (test* "手作り音色の負のDTは出力時に正規化せず拒否する"
                 (test-error <sxmml-value-error>
                             "invalid voice @tone operator(1) DT: -1 (expected an integer from 0 to 7)")
                 (raw-voice-data "raw-negative-dt" '(0 0) '(0 0 0 0 0 0 0 0 -1 0 0 0)))
          (test* "手作り音色の全出力項目上限の25バイトを維持する"
                 (string-join (append '("00") (make-list 8 "7f") (make-list 4 "df")
                                      (make-list 4 "9f") (make-list 4 "1f") (make-list 4 "ff") '("3f")) " ")
                 (string-join (take (string-split
                                    (section-hex (raw-voice-data "raw-upper-ok" '(7 7) upper) 'voice) " ") 26)
                              " "))
          (test* "未使用の手作り音色は出力時の検査対象に加えない"
                 '("00" "ff")
                 (take (string-split
                        (section-hex (raw-voice-data "raw-unused" '(0 0)
                                                     '(32 0 0 0 0 0 0 0 0 0 0 0) #f) 'voice) " ") 2))
          (test* "非出力項目DT2とSEGは手作り音色の生成バイトに影響しない"
                 (section-hex (raw-voice-data "raw-ignored-zero" '(0 0) (make-list 12 0)) 'voice)
                 (section-hex (raw-voice-data "raw-ignored-fields" '(0 0)
                                              '(0 0 0 0 0 0 0 0 0 4 16 0)) 'voice))
          (let1 registered-data
              (^[dt] (section-hex
                      (make-binary-data #"registered-dt-~dt" pmd-binary
                                        `((define-opn-voice tone (0 0)
                                            ,@(make-list 4 (list 0 0 0 0 0 0 0 0 dt)))
                                          (mml A "@{tone}o4c%12")) #f) 'voice))
            (test* "負数表記と非負表記のDTは同じ音色表を出力する"
                   #t (equal? (registered-data -1) (registered-data 5))))
          (test* "手作り音色の非整数は符号化前に値エラーとして拒否する"
                 (test-error <sxmml-value-error>
                             "invalid voice @tone operator(1) TL: 1/2 (expected an integer from 0 to 127)")
                 (raw-voice-data "raw-fraction" '(0 0) '(0 0 0 0 0 1/2 0 0 0 0 0 0))))

        (test* "公開pmd-binary出力の範囲違反は値エラーになる"
               (test-error <sxmml-value-error> "invalid FM volume: 128 (expected an integer from 0 to 127)")
               (make-binary-data "typed-volume" pmd-binary '((mml A "v128c"))))
        (test* "公開pmd-binary出力の未定義音色は参照エラーになる"
               (test-error <sxmml-reference-error> #/undefined voice/)
               (make-binary-data "typed-reference" pmd-binary '((mml A "@{UNKNOWN}c"))))

        (test* "通常トラックの直接IR入力で2個目のLを拒否する"
               (test-error <sxmml-mml-syntax-error> "loop command can be used only once in a track")
               (multiple-loop-output 'G))
        (test* "リズムの直接IR入力で2個目のLを拒否する"
               (test-error <sxmml-mml-syntax-error> "loop command can be used only once in a track")
               (multiple-loop-output 'L))
        (test* "入力名なしのライブラリ利用では ./a.m を既定値として記録する"
               '("./a.m" "./a.m")
               (parameterize ([sxmml-environment (make <sxmml-environment>)]
                              [sxmml-source-file #f])
                 (pmd-binary)
                 (list ((with-module sxmml.backend.pmd-binary %output-file-get))
                       (slot-ref (sxmml-environment) 'output))))
        (let1 source (source-output (work-file "library.sxl") '((mml G "c8")))
          (test* "ライブラリから直接生成しても入力名に対応する .m を使う"
                 (list (work-file "library.m") #t)
                 (parameterize ([sxmml-environment (make <sxmml-environment>)]
                                [sxmml-source-file source])
                   (pmd-binary)
                   (load source)
                   (sxmml-pmd-binary-output)
                   (list (slot-ref (sxmml-environment) 'output)
                         (file-exists? (work-file "library.m"))))))
        ;; 公開生成関数は、モード手続きを呼ばずにトラックを登録しても使える。
        (dolist [case (list (list "pmd-binary" sxmml-pmd-binary-output)
                           (list "pmd3-binary" sxmml-pmd3-binary-output)
                           (list "pmd4-binary" sxmml-pmd4-binary-output))]
          (let* ([name (car case)]
                 [source (source-output (work-file #"direct-~|name|.sxl") '((mml G "c8")))]
                 [output (work-file #"direct-~|name|.m")])
            (test* #"~name はモード手続きなしの直接生成でも .m を使う"
                   (list output #t)
                   (parameterize ([sxmml-environment (make <sxmml-environment>)]
                                  [sxmml-source-file source])
                     (define-track (A B C D E F G H I J L))
                     (load source)
                     ((cadr case))
                     (list (slot-ref (sxmml-environment) 'output) (file-exists? output))))))
        (dolist [mode '("pmd-binary" "pmd3-binary" "pmd4-binary")]
          (let* ([source (source-output (work-file #"voices-~|mode|.sxl")
                                      `(,(voice-form 'z 3) ,(voice-form 'a 1)
                                        ,(voice-form 'm 2) ,(voice-form 'unused 7)
                                        (mml A "@{m}c8@{z}c8@{a}c8")))]
                 [output (work-file #"voices-~|mode|.m")])
            ;; 番号は定義した全ての音色に id の辞書順で振る (#27)。a 0、m 1、unused 2、z 3
            (test* #"~mode は使用音色だけを番号順に出力する"
                   '(0 ((0 1) (1 2) (3 3)))
                   (list (car (run-sxlc source "-m" mode "-o" output))
                         (and (file-exists? output)
                              (voice-records output (string=? mode "pmd3-binary")))))
            (source-output source `((begin ,(voice-form 'z 3) ,(voice-form 'a 0)
                                          ,(voice-form 'y 2) ,(voice-form 'b 1))
                                   (mml A "@{z}c8@{y}c8@{b}c8@{a}c8")))
            (test* #"~mode は begin 内の音色にも別の番号を振る"
                   '(0 ((0 0) (1 1) (2 2) (3 3)))
                   (list (car (run-sxlc source "-m" mode "-o" output))
                         (voice-records output (string=? mode "pmd3-binary"))))
            (let1 before (file->bytevector output)
              (test* #"~mode の同じ入力は別プロセスでもバイト単位で一致する"
                     '(0 #t)
                     (list (car (run-sxlc source "-m" mode "-o" output))
                           (equal? before (file->bytevector output)))))
            ;; 定義した音色が 256 を超えたら、使った音色が 1 つでも失敗する (#27)
            (let ([source (source-output (work-file #"voices-257-~|mode|.sxl")
                                        `(,@(map (^i (voice-form (string->symbol (format "v~3,'0d" i)) 0))
                                                 (iota 257))
                                          (mml A "@{v000}c8")))]
                  [output (work-file #"voices-257-~|mode|.m")])
              (let1 result (run-sxlc source "-m" mode "-o" output)
                (test* #"~mode は音色が 257 個なら失敗する"
                       '(1 #t #f)
                       (list (car result)
                             (boolean (any #/too many voices: 257 \(max 256\)/ (cadr result)))
                             (file-exists? output)))))))
        ;; 命令列のバイト内容を固定する。期待値は MC.EXE との一致を主張せず、現在の出力を固定する。
        ;; 音高は (オクターブ - 1) * 16 + 音名、tick は解像度 24 の値。A トラックは位置 26 から始まる。
        (dolist [case
                 `(("音符・休符" "note" ((mml A "o3a8>c+4r8")) A
                    ;; o3a 12、o4c+ 24、休符 12
                    "29 0c 31 18 0f 0c 80")
                   ("255 tick を超える音符はタイで分割する" "long-note" ((mml A "o4c%255c%256")) A
                    "30 ff 30 ff fb 30 01 80")
                   ("255 tick を超える休符は休符を続けて分割する" "long-rest" ((mml A "o4r%255c8r%256")) A
                    "0f ff 30 0c 0f ff 0f 01 80")
                   ("タイ" "tie" ((mml A "o4c8&d8")) A
                    "30 0c fb 32 0c 80")
                   ("ゲートタイム・テンポ・デチューン・移調・パン" "control"
                    ((mml A "o4q4c8 t120c8 T200c8 D-2c8 K2c8 K*-1c8 p2c8")) A
                    ;; q4、t120 はタイマー値 183、T200、D-2、K2、K*-1、p2
                    "fe 04 30 0c fc b7 30 0c fc c8 30 0c fa fe ff 30 0c f5 02 30 0c e7 ff 30 0c ec 02 30 0c 80")
                   ("FM の音色・音量・音量変化" "fm" (,zero-voice (mml A "@0v100o4c8)c8(c8v*-2c8v*3c8")) A
                    ;; @0、v100、) は 4、( は -4、v*-2、v*3
                    "ff 00 fd 64 30 0c f4 30 0c f3 30 0c e2 02 30 0c e3 03 30 0c 80")
                   ;; a は 0、b は 1。@ は音色番号の表の番号を書く (#27)
                   ("FM の音色番号" "fm-voice-number" (,(voice-form 'b 0) ,(voice-form 'a 0) (mml A "@{b}o4c8")) A
                    "ff 01 30 0c 80")
                   ("SSG の音色・音量・音量変化・ミックス・ノイズ・ポルタメント" "ssg"
                    ((mml G "@1v10o4c8)c8P1w8c8/{c g}8")) G
                    ;; @1 は f0 の 4 値、) は 1、P1 は 07、w8、/{c g}8
                    "f0 02 ff 00 01 fd 0a 30 0c f4 30 0c ed 07 ee 08 30 0c da 30 37 0c 80")
                   ;; ポルタメントの音長は 1 バイトで、255 tick まで書ける (#57)
                   ("ポルタメントの音長の上限" "portamento-max" ((mml G "o4/{c g}%255")) G
                    "da 30 37 ff 80")
                   ("PCM の音色・音量・音量変化" "pcm" ((mml J "@1v100o4c8)c8(c8")) J
                    ;; ) は 16、( は -16
                    "ff 01 fd 64 30 0c f4 30 0c f3 30 0c 80")
                   ("PCM の音色の両端" "pcm-voice-range" ((mml J "@0o4c8@255c8")) J
                    "ff 00 30 0c ff ff 30 0c 80")
                   ("SSG のミックスの P2・P3" "ssg-mix" ((mml G "P2o4c8P3c8")) G
                    "ed 38 30 0c ed 3f 30 0c 80")
                   ("反復" "repeat" ((mml A "o4[c8d8]2")) A
                    ;; f9 は反復終了の次の位置 34、f8 は回数 2・2 と反復開始の次の位置 27
                    "f9 22 00 30 0c 32 0c f8 02 02 1b 00 80")
                   ("脱出を含む反復" "repeat-break" ((mml A "o4[c8:d8]2")) A
                    ;; f9 と f7 は反復終了の次の位置 37、f8 は回数 2・1
                    "f9 25 00 30 0c f7 25 00 32 0c f8 02 01 1b 00 80")
                   ;; 脱出のある反復の f8 の 3 バイト目は回数 - 1。
                   ;; 次の2件は、同じ MML の MC.EXE の生成物と命令列を照合した (#62)
                   ("脱出のある 1 回の反復" "once-repeat-break" ((mml A "o4[c:d]1")) A
                    "f9 25 00 30 18 f7 25 00 32 18 f8 01 00 1b 00 80")
                   ("1 回の反復" "once-repeat" ((mml A "o4[cd]1")) A
                    "f9 22 00 30 18 32 18 f8 01 01 1b 00 80")
                   ;; 外側も内側も2回。外側の回数・最終回の値を2・1とした期待値で、MC.EXEとは未照合。
                   ("入れ子の外側の脱出" "nested-repeat-break" ((mml A "o4[[c:d]2e:f]2")) A
                    "f9 34 00 f9 28 00 30 18 f7 28 00 32 18 f8 02 01 1e 00 34 18 f7 34 00 35 18 f8 02 01 1b 00 80")
                   ("音域の両端" "note-range" ((mml A "o1c8o8b8")) A
                    ;; o1c と o8b
                    "00 0c 7b 0c 80")
                   ("ループ" "loop" ((mml A "o4c8Ld8")) A
                    "30 0c f6 32 0c 80")
                   ("LFO の定義・切り替え" "lfo"
                    ((define-lfo L1 (1 2 -3 4)) (mml A "M{L1 A}Mto4c8M{L1 B}Mt{B}c8Mfc8Mf{B}c8")) A
                    ;; チャンネル A は f2・f1、B は bf・be。
                    "f2 01 02 fd 04 f1 01 30 0c bf 01 02 fd 04 be 01 30 0c f1 00 30 0c be 00 30 0c 80")
                   ("4 値と 6 値のエンベロープ" "envelope"
                    ((define-envelope E4 (1 -2 3 4)) (define-envelope E6 (1 2 3 4 5 6))
                     (mml G "E{E4}o4c8E{E6}c8")) G
                    ;; 6 値の 4・5 番目は 1 バイトにまとめる
                    "f0 01 fe 03 04 30 0c cd 01 02 03 54 06 30 0c 80")
                   ("リズムの K トラック" "rhythm-k" ((rhythm L "@b8")) K
                    "00 80")
                   ;; R トラックの末尾 12 バイトは、合計 tick、0、loop tick、0、メタ情報の位置の表、48 fe。
                   ;; 合計 tick は L トラックを含めた全トラックの最大値、loop tick は L のあるトラックの
                   ;; 最大値で、L のあるトラックがなければ 0 (#65)
                   ("リズムの R トラック" "rhythm-r" ((rhythm L "V40vb10pb2)b1@b8(b1!sr8")) R
                    ;; R は位置 38 から始まり、先頭の 2 バイトがパターンの位置 40。
                    ;; V40、vb10、pb2、)b1、@b8 と 12 tick、(b1、!s、休符。合計 tick 24、L がないので loop tick 0
                    "28 00 e8 28 ea 2a e9 22 e5 01 01 eb 01 0f 0c e5 01 ff eb 82 0f 0c ff 18 00 00 00 00 00 00 00 51 00 48 fe")
                   ;; 楽器別の音量の両端 0・31。バスは 1、スネアは 2 で、楽器 * 32 + 音量の 1 バイト (#102)
                   ;; パンの両端 1・3。バスの右は 21、スネアの中央は 43 (#102)
                   ("リズムのパンの両端" "rhythm-pan-range" ((rhythm L "pb1ps3@bs8")) R
                    "28 00 e9 21 e9 43 eb 03 0f 0c ff 0c 00 00 00 00 00 00 00 45 00 48 fe")
                   ("リズムの楽器別の音量の両端" "rhythm-volume-range" ((rhythm L "vb0vs31@bs8")) R
                    "28 00 ea 20 ea 5f eb 03 0f 0c ff 0c 00 00 00 00 00 00 00 45 00 48 fe")
                   ("ループを含むリズムの K トラック" "rhythm-loop-k" ((rhythm L "@b8L[@s8]2")) K
                    "00 f6 01 80")
                   ("ループと反復を含むリズムの R トラック" "rhythm-loop-r" ((rhythm L "@b8L[@s8]2")) R
                    ;; R は位置 40 から始まり、パターンの位置 44 とループの位置 49 を持つ。
                    ;; 合計 tick 36、loop tick 24
                    "2c 00 31 00 eb 01 0f 0c ff f9 39 00 eb 02 0f 0c f8 02 02 32 00 ff 24 00 00 00 18 00 00 00 52 00 48 fe")
                   ("リズムより短い他のトラック" "rhythm-longest" ((mml A "o4c8") (rhythm L "@b1")) R
                    ;; A は 12 tick、L は 96 tick。合計 tick は L の 96、L がないので loop tick 0
                    "2a 00 eb 01 0f 60 ff 60 00 00 00 00 00 00 00 43 00 48 fe")
                   ("リズムだけ" "rhythm-only" ((rhythm L "@b1")) R
                    ;; L は 96 tick。合計 tick は L の 96（これまでは 0 を書いていた）、L がないので loop tick 0
                    "28 00 eb 01 0f 60 ff 60 00 00 00 00 00 00 00 41 00 48 fe")
                   ;; L のあるトラックがなければ loop tick は 0。同じ内容を MC.EXE でコンパイルした生成物と
                   ;; 照合した。パンの 2・3・1 は左・中央・右 (#65、#109)
                   ("L のないリズム" "rhythm-no-loop" ((rhythm L "pb2@b8pb3@b8pb1@b8")) R
                    "28 00 e9 22 eb 01 0f 0c e9 23 eb 01 0f 0c e9 21 eb 01 0f 0c ff 24 00 00 00 00 00 00 00 4f 00 48 fe")
                   ;; 合計 tick と loop tick は、トラックごとに別々に最大を取る。A は 108 tick・loop 12 tick、
                   ;; B と L は 36 tick・loop 24 tick なので、合計 tick は A の 108、loop tick は B と L の 24。
                   ;; 同じ内容を MC.EXE でコンパイルした生成物と照合した (#65、#109)
                   ("合計 tick と loop tick の最大を別々のトラックから取る" "separate-max"
                    ((mml A "c1Lc8") (mml B "g8Lg4") (rhythm L "@b8L@b4")) R
                    "36 00 3b 00 eb 01 0f 0c ff eb 01 0f 18 ff 6c 00 00 00 18 00 00 00 54 00 48 fe"))]
          (test* #"pmd-binary の命令列: ~(car case)"
                 (list-ref case 4)
                 (section-hex (make-binary-data (cadr case) pmd-binary (caddr case)) (cadddr case))))
        ;; ヘッダ、空のトラック、R トラックの末尾、音色表、メタ情報の配置を、ファイル全体で固定する。
        (let* ([forms '((define-opn-voice 0 (2 5)
                          (31 1 2 3 4 5 1 6 7) (30 8 9 10 11 12 2 13 1)
                          (29 14 15 1 2 16 3 2 3) (28 3 4 5 6 17 0 4 2))
                        (define-metadata title "テスト")
                        (define-metadata composer "sxmml")
                        (define-metadata memo (list "m1" "m2"))
                        (mml A "@0o4c8Ld8"))]
               ;; 音色番号と、レジスタごとに OP1・OP3・OP2・OP4 の順の 24 バイト、FB/AL
               [voice (string-append "00 76 32 1d 24 05 10 0c 11 5f dd 9e 1c"
                                     " 01 0e 08 03 02 0f 09 04 43 21 ba 65 2a")]
               ;; A～F、G～I、J、K、R の各トラック
               [tracks "ff 00 30 0c f6 32 0c 80 80 80 80 80 80 80 80 80 80 80"])
          (test* "pmd-binary の生成物全体: PMD4 はメタ情報と R トラックの末尾 12 バイトを持つ"
                 (string-join
                  (list "00 1a 00 22 00 23 00 24 00 25 00 26 00 27 00 28 00 29 00 2a 00 2b 00 2c 00 38 00"
                        tracks
                        ;; リズムがないので、R トラックはこの 12 バイトだけ (#112)。
                        ;; 合計 tick 24、0、loop tick 12、0、メタ情報の位置の表 107、48 fe
                        "18 00 00 00 0c 00 00 00 6b 00 48 fe"
                        voice "00 ff"
                        ;; ppzfile・ppsfile・pcmfile は空、title は CP932、composer、arranger は空、memo 2 件
                        "00 00 00 83 65 83 58 83 67 00 73 78 6d 6d 6c 00 00 6d 31 00 6d 32 00"
                        ;; 各文字列の位置と 00 00
                        "54 00 55 00 56 00 57 00 5e 00 64 00 65 00 68 00 00 00")
                  " ")
                 (hex-bytes (make-binary-data "whole-pmd4" pmd-binary forms)))
          ;; リズムがなければ、R トラックにリズムのパターンを書かない。PMD4 の R トラックは末尾の
          ;; 12 バイトだけ、PMD3 は 0 バイトになる。次の4件は、従来 MC.EXE と照合した配置の期待値を
          ;; 基に、A・Cも1回にした。Aの f8 は1・0、Cは1・1で、位置と合計 tick 48は変わらない。
          ;; 変更後の入力は MC.EXE とは未照合 (#106)。
          (let1 forms-no-rhythm '((mml A "[c:d]1") (mml B "[c:d]1") (mml C "[cd]1") (mml D "[cd]1"))
            (dolist [case `((0 () "00 1a 00 2a 00 3a 00 47 00 54 00 55 00 56 00 57 00 58 00 59 00 5a 00 5b 00 67 00 f9 25 00 30 18 f7 25 00 32 18 f8 01 00 1b 00 80 f9 35 00 30 18 f7 35 00 32 18 f8 01 00 2b 00 80 f9 42 00 30 18 32 18 f8 01 01 3b 00 80 f9 4f 00 30 18 32 18 f8 01 01 48 00 80 80 80 80 80 80 80 80 30 00 00 00 00 00 00 00 6f 00 48 fe 00 ff 00 00 00 00 00 00 69 00 6a 00 6b 00 6c 00 6d 00 6e 00 00 00")
                            (1 ((define-metadata memo "memo1")) "00 1a 00 2a 00 3a 00 47 00 54 00 55 00 56 00 57 00 58 00 59 00 5a 00 5b 00 67 00 f9 25 00 30 18 f7 25 00 32 18 f8 01 00 1b 00 80 f9 35 00 30 18 f7 35 00 32 18 f8 01 00 2b 00 80 f9 42 00 30 18 32 18 f8 01 01 3b 00 80 f9 4f 00 30 18 32 18 f8 01 01 48 00 80 80 80 80 80 80 80 80 30 00 00 00 00 00 00 00 75 00 48 fe 00 ff 00 00 00 00 00 00 6d 65 6d 6f 31 00 69 00 6a 00 6b 00 6c 00 6d 00 6e 00 6f 00 00 00")
                            (2 ((define-metadata memo (list "memo1" "memo2"))) "00 1a 00 2a 00 3a 00 47 00 54 00 55 00 56 00 57 00 58 00 59 00 5a 00 5b 00 67 00 f9 25 00 30 18 f7 25 00 32 18 f8 01 00 1b 00 80 f9 35 00 30 18 f7 35 00 32 18 f8 01 00 2b 00 80 f9 42 00 30 18 32 18 f8 01 01 3b 00 80 f9 4f 00 30 18 32 18 f8 01 01 48 00 80 80 80 80 80 80 80 80 30 00 00 00 00 00 00 00 7b 00 48 fe 00 ff 00 00 00 00 00 00 6d 65 6d 6f 31 00 6d 65 6d 6f 32 00 69 00 6a 00 6b 00 6c 00 6d 00 6e 00 6f 00 75 00 00 00")
                            (3 ((define-metadata memo (list "memo1" "memo2" "memo3"))) "00 1a 00 2a 00 3a 00 47 00 54 00 55 00 56 00 57 00 58 00 59 00 5a 00 5b 00 67 00 f9 25 00 30 18 f7 25 00 32 18 f8 01 00 1b 00 80 f9 35 00 30 18 f7 35 00 32 18 f8 01 00 2b 00 80 f9 42 00 30 18 32 18 f8 01 01 3b 00 80 f9 4f 00 30 18 32 18 f8 01 01 48 00 80 80 80 80 80 80 80 80 30 00 00 00 00 00 00 00 81 00 48 fe 00 ff 00 00 00 00 00 00 6d 65 6d 6f 31 00 6d 65 6d 6f 32 00 6d 65 6d 6f 33 00 69 00 6a 00 6b 00 6c 00 6d 00 6e 00 6f 00 75 00 7b 00 00 00"))]
              (test* #"pmd-binary の生成物全体: リズムがなく、memo が ~(car case) 件"
                     (caddr case)
                     (hex-bytes (make-binary-data #"whole-memo-~(car case)" pmd-binary
                                                 (append (cadr case) forms-no-rhythm))))))
          (test* "pmd3-binary はリズムがなければ R トラックに何も書かない"
                 ""
                 (section-hex (make-binary-data "pmd3-no-rhythm" pmd3-binary '((mml A "c8"))) 'R))
          ;; 次の 2 件は、同じ内容の MML を MC.EXE でコンパイルした生成物とバイトまで同じ (#65、#109)
          (test* "pmd-binary の生成物全体: ループのあるトラックとリズム"
                 "00 1a 00 20 00 26 00 27 00 28 00 29 00 2a 00 2b 00 2c 00 2d 00 2e 00 32 00 4c 00 30 60 f6 30 0c 80 37 0c f6 37 18 80 80 80 80 80 80 80 80 80 00 f6 01 80 36 00 3b 00 eb 01 0f 0c ff eb 01 0f 18 ff 6c 00 00 00 18 00 00 00 54 00 48 fe 00 ff 00 00 00 00 00 00 4e 00 4f 00 50 00 51 00 52 00 53 00 00 00"
                 (hex-bytes (make-binary-data "whole-loop-rhythm" pmd-binary
                                             '((mml A "c1Lc8") (mml B "g8Lg4") (rhythm L "@b8L@b4")))))
          (test* "pmd-binary の生成物全体: ループのないリズム"
                 "00 1a 00 1b 00 1c 00 1d 00 1e 00 1f 00 20 00 21 00 22 00 23 00 24 00 26 00 47 00 80 80 80 80 80 80 80 80 80 80 00 80 28 00 e9 22 eb 01 0f 0c e9 23 eb 01 0f 0c e9 21 eb 01 0f 0c ff 24 00 00 00 00 00 00 00 4f 00 48 fe 00 ff 00 00 00 00 00 00 49 00 4a 00 4b 00 4c 00 4d 00 4e 00 00 00"
                 (hex-bytes (make-binary-data "whole-rhythm-no-loop" pmd-binary
                                             '((rhythm L "pb2@b8pb3@b8pb1@b8")))))
          (test* "pmd3-binary の生成物全体: メタ情報・R トラックの末尾・音色表の終端を持たない"
                 (string-join
                  (list "00 1a 00 22 00 23 00 24 00 25 00 26 00 27 00 28 00 29 00 2a 00 2b 00 2c 00 2c 00"
                        tracks voice)
                  " ")
                 (hex-bytes (make-binary-data "whole-pmd3" pmd3-binary forms))))
        ;; PMD の音高の 1 バイトで表せない音高は、範囲外の音高と許される範囲を示して失敗する。
        (dolist [case '(("上側" "o8c>c" "o9c" "note")
                       ("上側" "o8b>e-" "o9e-" "note")
                       ("下側" "o1c<c" "o0c" "note")
                       ("下側" "o1c<<b" "o-1b" "note")
                       ("ポルタメントの終了音" "o8/{b >c}8" "o9c" "portamento end note")
                       ("ポルタメントの開始音" "o0/{b >c}8" "o0b" "portamento start note"))]
          (let* ([source (source-output (work-file #"note-range-~(caddr case).sxl")
                                       `((mml A ,(cadr case))))]
                 [output (work-file #"note-range-~(caddr case).m")]
                 [result (run-sxlc source "-m" "pmd-binary" "-o" output)])
            (test* #"音高が~(car case)の範囲外なら失敗する: ~(cadr case)"
                   '(1 #t #f)
                   (list (car result)
                         (boolean (any (string->regexp
                                        (regexp-quote #"invalid ~(cadddr case): ~(caddr case) (expected o1c - o8b)"))
                                       (cadr result)))
                         (file-exists? output)))))
        ;; ポルタメントの音長は 1 バイトなので、255 tick を超えると、音長と許される範囲を示して
        ;; 失敗する。これまでは内部のエラーになっていた (#57)
        (dolist [case '(("pmd-binary" "256") ("pmd-binary" "300") ("pmd3-binary" "300"))]
          (let* ([mode (car case)]
                 [ticks (cadr case)]
                 [source (source-output (work-file #"portamento-~|mode|-~|ticks|.sxl")
                                       `((mml G ,#"o4/{c g}%~ticks")))]
                 [output (work-file #"portamento-~|mode|-~|ticks|.m")]
                 [result (run-sxlc source "-m" mode "-o" output)])
            (test* #"ポルタメントの音長が 255 tick を超えると失敗する: ~mode %~ticks"
                   '(1 #t #f)
                   (list (car result)
                         (boolean (any (string->regexp
                                        (regexp-quote #"invalid portamento ticks: ~ticks (expected an integer from 1 to 255)"))
                                       (cadr result)))
                         (file-exists? output)))))
        ;; リズムの音量・パンの誤りは、綴りの正しいメッセージで失敗する (#57)
        (dolist [case '(("volume" "V64 @b8" "invalid rhythm volume: 64 (expected an integer from 0 to 63)") ("volume-change" ")b32 @b8" "invalid relative rhythm volume: 32 (expected an integer from -31 to 31)"))]
          (let* ([source (source-output (work-file #"rhythm-param-~(car case).sxl")
                                       `((rhythm L ,(cadr case))))]
                 [output (work-file #"rhythm-param-~(car case).m")]
                 [result (run-sxlc source "-m" "pmd-binary" "-o" output)])
            (test* #"リズムの値の誤りのメッセージ: ~(cadr case)"
                   '(1 #t #f)
                   (list (car result)
                         (boolean (any (string->regexp
                                        (regexp-quote (caddr case)))
                                       (cadr result)))
                         (file-exists? output)))))
        ;; リズムの楽器別の音量は 0 から 31、パンは 1（右）・2（左）・3（中央）で、範囲外は書いた値と
        ;; 範囲を示して失敗する。これまで音量は楽器のビットにはみ出して黙って誤った生成物になり、
        ;; パンは符号化した値を表示していた (#102)
        (dolist [case '(("volume-32" "vb32 @b8" "invalid rhythm instrument volume: 32 (expected an integer from 0 to 31)")
                        ("volume-40" "v{s}40 @s8" "invalid rhythm instrument volume: 40 (expected an integer from 0 to 31)")
                        ("pan-0" "pb0 @b8" "invalid rhythm pan: 0 (expected an integer from 1 to 3)")
                        ("pan-4" "pb4 @b8" "invalid rhythm pan: 4 (expected an integer from 1 to 3)"))]
          (let* ([source (source-output (work-file #"rhythm-range-~(car case).sxl")
                                       `((rhythm L ,(cadr case))))]
                 [output (work-file #"rhythm-range-~(car case).m")]
                 [result (run-sxlc source "-m" "pmd-binary" "-o" output)])
            (test* #"リズムの範囲外の値は失敗する: ~(cadr case)"
                   '(1 #t #f)
                   (list (car result)
                         (boolean (any (string->regexp (regexp-quote (caddr case))) (cadr result)))
                         (file-exists? output)))))
        ;; メタデータの値は -m pmd と同じ規則で検査する。PMD3 はメタデータを書かないので
        ;; 検査しない (#13)
        (let1 source (source-output (work-file "meta-title-number.sxl")
                                   '((define-metadata title 1) (mml A "c8")))
          (dolist [case '(("pmd-binary" (1 #t #f)) ("pmd3-binary" (0 #f #t)))]
            (let* ([output (work-file #"meta-title-number-~(car case).m")]
                   [result (run-sxlc source "-m" (car case) "-o" output)])
              (test* #"~(car case) はメタデータの title の値が文字列でなければ、書くときに失敗する"
                     (cadr case)
                     (list (car result)
                           (boolean (any #/invalid metadata value: title is 1 \(expected a string\)/
                                         (cadr result)))
                           (file-exists? output))))))
        ;; メタデータがなければ、音色表の終端に続けて、memo 以外の 6 項目の空文字列、その位置の表、
        ;; 終端の 00 00 を書く。memo は書かない。空のリストの memo も、memo がない場合と同じ。
        ;; メタデータのない MML を MC.EXE でコンパイルした生成物のメタ情報と同じ形 (#109)
        (dolist [case '(("meta-none" ((mml A "c8")))
                        ("meta-memo-empty" ((define-metadata memo '()) (mml A "c8"))))]
          (test* #"pmd-binary の空のメタ情報: ~(car case)"
                 "00 ff 00 00 00 00 00 00 35 00 36 00 37 00 38 00 39 00 3a 00 00 00"
                 (section-hex (make-binary-data (car case) pmd-binary (cadr case)) 'voice)))
        ;; memo が 1 件なら、6 項目の空文字列に続けて memo を 1 件書き、位置の表は 7 件。
        ;; 文字列 1 つの memo は、要素 1 つのリストと同じ (#109)
        (dolist [case '(("meta-memo-string" ((define-metadata memo "m") (mml A "c8")))
                        ("meta-memo-list" ((define-metadata memo (list "m")) (mml A "c8"))))]
          (test* #"pmd-binary の 1 件の memo: ~(car case)"
                 "00 ff 00 00 00 00 00 00 6d 00 35 00 36 00 37 00 38 00 39 00 3a 00 3b 00 00 00"
                 (section-hex (make-binary-data (car case) pmd-binary (cadr case)) 'voice)))
        (test* "pmd-binary はメタデータの memo の要素が文字列でなければ失敗する"
               (test-error <sxmml-value-error> "invalid metadata value: memo is (\"a\" 1) (expected a string or a list of strings)")
               (make-binary-data "meta-memo-element" pmd-binary
                                '((define-metadata memo (list "a" 1)) (mml A "c8"))))
        ;; LFO・エンベロープは -m pmd と同じ規則で検査する。PMD3 だけの検査は、チャンネル B と
        ;; 5・6 値のエンベロープである (#28)
        (let ([caught (^[name mode forms]
                        (guard (e [else (condition-message e)])
                          (make-binary-data name mode forms)
                          'generated))])
          (dolist [case `(("lfo-type" ,pmd-binary
                           ((define-lfo L1 sw (1 2 3 4)) (mml A "M{L1 A}c8"))
                           "unsupported LFO type: sw in L1 (supported: default, triangle)")
                          ("envelope-count" ,pmd-binary
                           ((define-envelope E1 (1 2 3)) (mml G "E{E1}c8"))
                           "invalid envelope values: E1 has 3 values (expected 4, 5 or 6)")
                          ("lfo-inexact" ,pmd-binary
                           ((define-lfo L1 (1 1.5 3 4)) (mml A "M{L1 A}c8"))
                           "invalid LFO L1 value 2: 1.5 (expected an integer from 0 to 255)")
                          ("pmd3-lfo-b" ,pmd3-binary
                           ((define-lfo L1 (1 2 3 4)) (mml A "M{L1 B}c8"))
                           "invalid LFO channel: B")
                          ("pmd3-lfo-switch-b" ,pmd3-binary
                           ((mml A "Mt{B}c8"))
                           "invalid LFO channel: B")
                          ("pmd3-envelope-6" ,pmd3-binary
                           ((define-envelope E6 (1 2 3 4 5 6)) (mml G "E{E6}c8"))
                           "unsupported envelope values in PMD3: E6 has 6 values (expected 4)"))]
            (test* #"pmd-binary の LFO・エンベロープの検査: ~(car case)"
                   (cadddr case)
                   (caught #"check-~(car case)" (cadr case) (caddr case))))

          (test* "pmd-binary の LFO の triangle は default と同じバイト内容になる"
                 (section-hex (make-binary-data "lfo-default" pmd-binary
                                               '((define-lfo L1 (1 2 -3 4)) (mml A "M{L1 A}c8")))
                              'A)
                 (section-hex (make-binary-data "lfo-triangle" pmd-binary
                                               '((define-lfo L1 triangle (1 2 -3 4)) (mml A "M{L1 A}c8")))
                              'A))
          (let* ([source (source-output (work-file "check-cli.sxl")
                                       '((define-lfo L1 sw (1 2 3 4)) (mml A "M{L1 A}c8")))]
                 [output (work-file "check-cli.m")]
                 [result (run-sxlc source "-m" "pmd-binary" "-o" output)])
            (test* "pmd-binary の LFO の種類の誤りは、終了値 1 で生成物を作らない"
                   '(1 #t #f)
                   (list (car result)
                         (boolean (any #/unsupported LFO type: sw in L1/ (cadr result)))
                         (file-exists? output)))))
        ;; PCM の音色は 0 から 255 までの整数で、それ以外は書かれた値を示して失敗する (#58)
        (dolist [case '(("@256" "256") ("@{1.5}" "1.5") ("@{x}" "x"))]
          (let* ([source (source-output (work-file #"pcm-voice-~(cadr case).sxl")
                                       `((mml J ,#"~(car case)c8")))]
                 [output (work-file #"pcm-voice-~(cadr case).m")]
                 [result (run-sxlc source "-m" "pmd-binary" "-o" output)])
            (test* #"PCM の音色が 0 から 255 までの整数でなければ失敗する: ~(car case)"
                   '(1 #t #f)
                   (list (car result)
                         (boolean (any (string->regexp
                                        #"invalid PCM voice number: ~(regexp-quote (cadr case)) \\(expected an integer from 0 to 255\\)$")
                                       (cadr result)))
                         (file-exists? output)))))
        ;; SSG のミックスは 1 から 3 までで、範囲外は値と範囲を示して失敗する (#55)
        (dolist [value '("0" "4")]
          (let* ([source (source-output (work-file #"ssg-mix-~|value|.sxl")
                                       `((mml G ,#"P~|value|c8")))]
                 [output (work-file #"ssg-mix-~|value|.m")]
                 [result (run-sxlc source "-m" "pmd-binary" "-o" output)])
            (test* #"SSG のミックスが 1 から 3 までの範囲外なら失敗する: P~value"
                   '(1 #t #f)
                   (list (car result)
                         (boolean (any (string->regexp
                                        (regexp-quote #"invalid output switch: ~value (expected an integer from 1 to 3)"))
                                       (cadr result)))
                         (file-exists? output)))))
        ;; 生成物は 65535 バイトまで、PMD4 の合計 tick は 65535 までとし、超えたら失敗する。
        ;; PMD3 の生成物は 2 * 音符の数 + 38 バイトなので、1 バイトの ) と 32748 個で上限になる。
        (let1 repeat-mml (^[n unit] (apply string-append (make-list n unit)))
          (dolist [case `(("2 トラックの合計が上限を超える" "pmd-binary" "sum"
                           ((mml A ,(repeat-mml 9000 "c8d8")) (mml B ,(repeat-mml 9000 "c8d8")))
                           "PMD data exceeds the size limit (65535 bytes) in track B")
                          ("1 トラックが作業用のバッファを超える" "pmd-binary" "track"
                           ((mml A ,(repeat-mml 25000 "c8d8")))
                           "PMD data exceeds the size limit (65535 bytes) in track A")
                          ("1 つの長い音符が上限を超える" "pmd-binary" "long-note"
                           ((mml A "c%20000000"))
                           "PMD data exceeds the size limit (65535 bytes) in track A")
                          ("1 つの長い休符が上限を超える" "pmd-binary" "long-rest"
                           ((mml G "r%20000000"))
                           "PMD data exceeds the size limit (65535 bytes) in track G")
                          ("音符を含まない命令が上限を超える" "pmd-binary" "commands"
                           ((mml J ,(repeat-mml 33000 "v10")))
                           "PMD data exceeds the size limit (65535 bytes) in track J")
                          ("リズムが上限を超える" "pmd-binary" "rhythm"
                           ((rhythm L ,(repeat-mml 33000 "!b")))
                           "PMD data exceeds the size limit (65535 bytes) in track L")
                          ("メタ情報が上限を超える" "pmd-binary" "metadata"
                           ((define-metadata memo (list ,(make-string 70000 #\a))) (mml A "c8"))
                           "PMD data exceeds the size limit (65535 bytes) in metadata")
                          ;; memo の文字列は約 64000 バイトで上限に収まり、位置の表で超える
                          ("メタ情報の位置の表が上限を超える" "pmd-binary" "metadata-table"
                           ((define-metadata memo ,(cons 'list (make-list 1000 (make-string 63 #\a)))) (mml A "c8"))
                           "PMD data exceeds the size limit (65535 bytes) in metadata")
                          ;; 音色表がなければ 65520 バイトに収まる
                          ("音色表が上限を超える" "pmd3-binary" "voice-table"
                           (,zero-voice (mml A ,(string-append "@0" (repeat-mml 32740 "c8"))))
                           "PMD data exceeds the size limit (65535 bytes) in voice table")
                          ("65535 バイトの生成物は成功する" "pmd3-binary" "size-max"
                           ((mml A ,(string-append ")" (repeat-mml 32748 "c8"))))
                           65535)
                          ("65536 バイトの生成物は失敗する" "pmd3-binary" "size-over"
                           ((mml A ,(repeat-mml 32749 "c8")))
                           "PMD data exceeds the size limit (65535 bytes)")
                          ("合計 65535 tick は成功する" "pmd-binary" "ticks-max"
                           ((mml A "c%65535"))
                           #t)
                          ("合計 65536 tick は失敗する" "pmd-binary" "ticks-over"
                           ((mml A "c%65536"))
                           "total ticks exceed the limit: 65536 (max 65535)")
                          ("合計 67200 tick は失敗する" "pmd-binary" "ticks-67200"
                           ((mml A "c%67200"))
                           "total ticks exceed the limit: 67200 (max 65535)")
                          ;; L トラックの長さも合計 tick に入る (#65)
                          ("リズムの合計 65536 tick は失敗する" "pmd-binary" "ticks-rhythm-over"
                           ((rhythm L "@b%65536"))
                           "total ticks exceed the limit: 65536 (max 65535)")
                          ("PMD3 は合計 tick を書かないので 67200 tick も成功する" "pmd3-binary" "ticks-pmd3"
                           ((mml A "c%67200"))
                           #t))]
            (let* ([source (source-output (work-file #"limit-~(caddr case).sxl") (cadddr case))]
                   [output (work-file #"limit-~(caddr case).m")]
                   [expected (list-ref case 4)]
                   [result (run-sxlc source "-m" (cadr case) "-O" "0" "-o" output)])
              (test* #"~(car case)"
                     (if (string? expected) '(1 #t #f) (list 0 expected))
                     (if (string? expected)
                       (list (car result)
                             (boolean (any (string->regexp (regexp-quote expected)) (cadr result)))
                             (file-exists? output))
                       (list (car result)
                             (and (file-exists? output)
                                  (if (integer? expected) (file-size output) #t))))))))
      )
    (remove-directory* workdir)))

(test-end :exit-on-failure #t)
