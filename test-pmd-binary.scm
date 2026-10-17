;;;
;;; Test sxmml_pmd_binary
;;;

(use gauche.test)

(test-start "sxmml_pmd_binary")
(use sxmml.backend.pmd-binary)
(test-module 'sxmml.backend.pmd-binary)
(use sxmml)
(use gauche.process)
(use gauche.uvector)
(use file.util)

(let* ([sxlc (or (sys-getenv "SXMML_SXLC")
                 (find-file-in-paths "sxlc" :paths (string-split (sys-getenv "PATH") #\:))
                 (error "sxlc が見つかりません。SXMML_SXLC にテスト対象の CLI を指定してください。"))]
       [gosh (car (os-command-line))]
       ;; 子プロセスでも、テスト本体と同じ sxmml と当バックエンドを使う。
       [load-options (map (^p (string-append "-I" (sys-normalize-pathname p :absolute #t)))
                          (reverse *load-path*))]
       [workdir (sys-mkdtemp (build-path (temporary-directory) "sxmml-pmd-binary-test"))])
  (define (work-file name) (build-path workdir name))
  (define (write-source path forms)
    (with-output-to-file path (^[] (for-each write forms)))
    path)
  (define (run-sxlc source . options)
    (let* ([status 0]
           [lines (process-output->string-list
                   `(,gosh ,@load-options ,sxlc ,@options ,source)
                   :error :merge
                   :on-abnormal-exit (^p (set! status (process-exit-status p))))])
      (list (sys-wait-exit-status status) lines)))
  (define (output-report result)
    (list (car result)
          (and-let1 m (any #/^Translate: .* => (.*)$/ (cadr result)) (m 1))
          (and-let1 m (any #/^<(.*)>$/ (cadr result)) (m 1))))
  (define post-command "printf '<%s>\\n'")
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
  ;; ファイル名と内容を比較して、既存ファイルの変更や一時ファイルの残留を検出する。
  (define (snapshot dir)
    (map (^[name] (let1 path (build-path dir name)
                  (cons name (if (file-is-directory? path)
                               (snapshot path)
                               (file->bytevector path)))))
         (sort (directory-list dir :children? #t) string<?)))
  ;; ヘッダの 13 個の位置が指す区間の順。PMD4 の音色表の区間は、続くメタ情報を含む。
  (define binary-sections '(A B C D E F G H I J K R voice))
  ;; モードを選んでソースを読み込み、このプロセスで生成した .m の内容を返す。
  (define (generate-binary name mode forms)
    (let1 source (write-source (work-file #"bytes-~|name|.sxl") forms)
      (parameterize ([sxmml-environment (make <sxmml-environment>)]
                     [sxmml-source-file source])
        (mode)
        ;; sxlc の既定の -O 1 と同じ最適化を使う。
        (slot-set! (sxmml-environment) 'optimize #t)
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
  (unwind-protect
      (begin
        (unless (file-is-readable? sxlc)
          (error "テスト対象の CLI を読み込めません。SXMML_SXLC を確認してください。" sxlc))
        (dolist [mode '(pmd-binary pmd3-binary pmd4-binary)]
          (let* ([name (symbol->string mode)]
                 [source (write-source (work-file #"~|name|.sxl") '((mml G "c8")))]
                 [output (work-file #"~|name|.m")])
            (test* #"~mode は既定の .m を生成して同じ名前を表示・後処理へ渡す"
                   (list 0 output output #t)
                   (append (output-report (run-sxlc source "-m" name "-c" post-command))
                           (list (file-exists? output))))
            (let1 explicit (work-file #"~|name| ; output.bin")
              (test* #"~mode は -o の名前を一引数で後処理へ渡す"
                     (list 0 explicit explicit #t)
                     (append (output-report (run-sxlc source "-m" name "-o" explicit
                                                     "-c" post-command))
                             (list (file-exists? explicit)))))
            ;; -m とソース中のモード指定の両方で、既定出力の衝突を検査する。
            (dolist [cli? '(#t #f)]
              (let* ([dir (work-file #"~|name|-same-~cli?")]
                     [input (build-path dir "input.m")])
                (make-directory* dir)
                (write-source input `((sxmml-mode ,mode) (mml G "c8")))
                (let* ([before (snapshot dir)]
                       [result (apply run-sxlc input (if cli? (list "-m" name) '()))])
                  (test* #"~mode は入力と既定出力が同じ場合に上書きしない（-m: ~cli?）"
                         '(2 #t #t)
                         (list (car result)
                               (boolean (any #/^output file is the same as the source file:/
                                             (cadr result)))
                               (equal? before (snapshot dir)))))))))

        (let* ([source (write-source (work-file "metadata.sxl")
                                    `((define-metadata filename ,(work-file "obsolete.m"))
                                      (mml G "c8")))]
               [output (work-file "metadata.m")]
               [result (run-sxlc source "-m" "pmd-binary" "-c" post-command)])
          (test* "filename は警告して無視し、既定の出力先を使う"
                 (list 0 output output #t #t #f)
                 (append (output-report result)
                         (list (boolean (any #/^warning: metadata 'filename' is ignored/
                                             (cadr result)))
                               (file-exists? output)
                               (file-exists? (work-file "obsolete.m"))))))
        (let* ([source (write-source (work-file "slot.sxl")
                                    `((slot-set! (sxmml-environment) 'output ,(work-file "source-slot.m"))
                                      (mml G "c8")))]
               [output (work-file "slot.m")])
          (test* "ソース中の output の変更より CLI の出力先決定を優先する"
                 (list 0 output output #t #f)
                 (append (output-report (run-sxlc source "-m" "pmd-binary" "-c" post-command))
                         (list (file-exists? output) (file-exists? (work-file "source-slot.m"))))))
        (let* ([source (write-source (work-file "switch.sxl")
                                    '((sxmml-mode pmd) (sxmml-mode pmd-binary) (mml G "c8")))]
               [output (work-file "switch.m")])
          (test* "ソースモードからバイナリモードへ切り替えても .m を生成する"
                 (list 0 output output #t #f)
                 (append (output-report (run-sxlc source "-c" post-command))
                         (list (file-exists? output) (file-exists? (work-file "switch.mml"))))))

        (test* "入力名なしのライブラリ利用では ./a.m を既定値として記録する"
               '("./a.m" "./a.m")
               (parameterize ([sxmml-environment (make <sxmml-environment>)]
                              [sxmml-source-file #f])
                 (pmd-binary)
                 (list ((with-module sxmml.backend.pmd-binary %output-file-get))
                       (slot-ref (sxmml-environment) 'output))))
        (let1 source (write-source (work-file "library.sxl") '((mml G "c8")))
          (test* "ライブラリから直接生成しても入力名に対応する .m を使う"
                 (list (work-file "library.m") #t)
                 (parameterize ([sxmml-environment (make <sxmml-environment>)]
                                [sxmml-source-file source])
                   (pmd-binary)
                   (load source)
                   (sxmml-pmd-binary-generate)
                   (list (slot-ref (sxmml-environment) 'output)
                         (file-exists? (work-file "library.m"))))))

        ;; 公開生成関数は、モード手続きを呼ばずにトラックを登録しても使える。
        (dolist [case (list (list "pmd-binary" sxmml-pmd-binary-generate)
                           (list "pmd3-binary" sxmml-pmd3-binary-generate)
                           (list "pmd4-binary" sxmml-pmd4-binary-generate))]
          (let* ([name (car case)]
                 [source (write-source (work-file #"direct-~|name|.sxl") '((mml G "c8")))]
                 [output (work-file #"direct-~|name|.m")])
            (test* #"~name はモード手続きなしの直接生成でも .m を使う"
                   (list output #t)
                   (parameterize ([sxmml-environment (make <sxmml-environment>)]
                                  [sxmml-source-file source])
                     (define-track (A B C D E F G H I J L))
                     (load source)
                     ((cadr case))
                     (list (slot-ref (sxmml-environment) 'output) (file-exists? output))))))

        (let* ([source (write-source (work-file "priority.sxl")
                                    '((sxmml-mode pmd4-binary) (mml G "c8")))]
               [output (work-file "priority.m")]
               [result (run-sxlc source "-m" "pmd3-binary")])
          (test* "-m のバイナリモードはソース中の指定より優先される"
                 '(0 #t #t)
                 (list (car result)
                       (boolean (member "Processing Mode: pmd3-binary" (cadr result)))
                       (let1 other (work-file "pmd3-binary.m")
                         (and (file-exists? output) (file-exists? other)
                              (equal? (file->bytevector output) (file->bytevector other)))))))

        (dolist [mode '("pmd-binary" "pmd3-binary" "pmd4-binary")]
          (let* ([source (write-source (work-file #"voices-~|mode|.sxl")
                                      `(,(voice-form 'z 3) ,(voice-form 'a 1)
                                        ,(voice-form 'm 2) ,(voice-form 'unused 7)
                                        (mml A "@{m}c8@{z}c8@{a}c8")))]
                 [output (work-file #"voices-~|mode|.m")])
            (test* #"~mode は使用音色だけを番号順に出力する"
                   '(0 ((0 3) (1 1) (2 2)))
                   (list (car (run-sxlc source "-m" mode "-o" output))
                         (and (file-exists? output)
                              (voice-records output (string=? mode "pmd3-binary")))))
            ;; begin 内の番号重複は上流の既知の問題。ここでは出力順だけを固定する。
            (write-source source `((begin ,(voice-form 'z 3) ,(voice-form 'a 0)
                                          ,(voice-form 'y 2) ,(voice-form 'b 1))
                                   (mml A "@{z}c8@{y}c8@{b}c8@{a}c8")))
            (test* #"~mode は同じ番号の音色を ID 順に出力する"
                   '(0 ((0 0) (0 1) (0 2) (0 3)))
                   (list (car (run-sxlc source "-m" mode "-o" output))
                         (voice-records output (string=? mode "pmd3-binary"))))
            (let1 before (file->bytevector output)
              (test* #"~mode の同じ入力は別プロセスでもバイト単位で一致する"
                     '(0 #t)
                     (list (car (run-sxlc source "-m" mode "-o" output))
                           (equal? before (file->bytevector output)))))))

        ;; 命令列のバイト内容を固定する。期待値は MC.EXE との一致を主張せず、現在の出力を固定する。
        ;; 既知の欠陥（#55〜#58、#61、#62、#65）に当たる入力は使わない。
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
                   ("SSG の音色・音量・音量変化・ミックス・ノイズ・ポルタメント" "ssg"
                    ((mml G "@1v10o4c8)c8P1w8c8/{c g}8")) G
                    ;; @1 は f0 の 4 値、) は 1、P1 は 07、w8、/{c g}8
                    "f0 02 ff 00 01 fd 0a 30 0c f4 30 0c ed 07 ee 08 30 0c da 30 37 0c 80")
                   ("PCM の音色・音量・音量変化" "pcm" ((mml J "@1v100o4c8)c8(c8")) J
                    ;; ) は 16、( は -16
                    "ff 01 fd 64 30 0c f4 30 0c f3 30 0c 80")
                   ("反復" "repeat" ((mml A "o4[c8d8]2")) A
                    ;; f9 は反復終了の次の位置 34、f8 は回数 2・2 と反復開始の次の位置 27
                    "f9 22 00 30 0c 32 0c f8 02 02 1b 00 80")
                   ("脱出を含む反復" "repeat-break" ((mml A "o4[c8:d8]2")) A
                    ;; f9 と f7 は反復終了の次の位置 37、f8 は回数 2・1
                    "f9 25 00 30 0c f7 25 00 32 0c f8 02 01 1b 00 80")
                   ("無限反復" "repeat-infinite" ((mml A "o4[c8]0")) A
                    "f9 20 00 30 0c f8 00 00 1b 00 80")
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
                   ("リズムの R トラック" "rhythm-r" ((rhythm L "V40vb10pb2)b1@b8(b1!sr8")) R-body
                    ;; R は位置 38 から始まり、先頭の 2 バイトがパターンの位置 40。
                    ;; V40、vb10、pb2、)b1、@b8 と 12 tick、(b1、!s、休符
                    "28 00 e8 28 ea 2a e9 22 e5 01 01 eb 01 0f 0c e5 01 ff eb 82 0f 0c ff")
                   ("ループを含むリズムの K トラック" "rhythm-loop-k" ((rhythm L "@b8L[@s8]2")) K
                    "00 f6 01 80")
                   ("ループと反復を含むリズムの R トラック" "rhythm-loop-r" ((rhythm L "@b8L[@s8]2")) R-body
                    ;; R は位置 40 から始まり、パターンの位置 44 とループの位置 49 を持つ
                    "2c 00 31 00 eb 01 0f 0c ff f9 39 00 eb 02 0f 0c f8 02 02 32 00 ff"))]
          ;; R-body は R トラックの末尾 12 バイトを除く。末尾の tick には L トラックの長さが入らない
          ;; 欠陥（#65）があるため、末尾はリズムを含まないファイル全体の検査で固定する。
          (let1 section (cadddr case)
            (test* #"pmd-binary の命令列: ~(car case)"
                   (list-ref case 4)
                   (let1 hex (section-hex (generate-binary (cadr case) pmd-binary (caddr case))
                                          (if (eq? section 'R-body) 'R section))
                     (if (eq? section 'R-body)
                       (substring hex 0 (- (string-length hex) (* 12 3)))
                       hex)))))

        ;; ヘッダ、空のトラック、R トラックの末尾、音色表、メタ情報の配置を、ファイル全体で固定する。
        (let* ([forms '((define-opn-voice 0 (2 5)
                          (31 1 2 3 4 5 1 6 7) (30 8 9 10 11 12 2 13 1)
                          (29 14 15 1 2 16 3 2 3) (28 3 4 5 6 17 0 4 2))
                        (define-metadata title "テスト")
                        (define-metadata composer "sxmml")
                        (define-metadata memo ("m1" "m2"))
                        (mml A "@0o4c8Ld8"))]
               ;; 音色番号と、レジスタごとに OP1・OP3・OP2・OP4 の順の 24 バイト、FB/AL
               [voice (string-append "00 76 32 1d 24 05 10 0c 11 5f dd 9e 1c"
                                     " 01 0e 08 03 02 0f 09 04 43 21 ba 65 2a")]
               ;; A～F、G～I、J、K、R の各トラック
               [tracks (string-append "ff 00 30 0c f6 32 0c 80 80 80 80 80 80 80 80 80 80 80"
                                      " 2e 00 ff")])
          (test* "pmd-binary の生成物全体: PMD4 はメタ情報と R トラックの末尾 12 バイトを持つ"
                 (string-join
                  (list "00 1a 00 22 00 23 00 24 00 25 00 26 00 27 00 28 00 29 00 2a 00 2b 00 2c 00 3b 00"
                        tracks
                        ;; 合計 tick 24、0、loop tick 12、0、メタ情報の位置の表 110、48 fe
                        "18 00 00 00 0c 00 00 00 6e 00 48 fe"
                        voice "00 ff"
                        ;; ppzfile・ppsfile・pcmfile は空、title は CP932、composer、arranger は空、memo 2 件
                        "00 00 00 83 65 83 58 83 67 00 73 78 6d 6d 6c 00 00 6d 31 00 6d 32 00"
                        ;; 各文字列の位置と 00 00
                        "57 00 58 00 59 00 5a 00 61 00 67 00 68 00 6b 00 00 00")
                  " ")
                 (hex-bytes (generate-binary "whole-pmd4" pmd-binary forms)))
          (test* "pmd3-binary の生成物全体: メタ情報・R トラックの末尾・音色表の終端を持たない"
                 (string-join
                  (list "00 1a 00 22 00 23 00 24 00 25 00 26 00 27 00 28 00 29 00 2a 00 2b 00 2c 00 2f 00"
                        tracks voice)
                  " ")
                 (hex-bytes (generate-binary "whole-pmd3" pmd3-binary forms))))

        (dolist [case '(("read" "(mml G \"c8\"" "out.m" "読み込み")
                       ("generate" "(mml A \"@{undefined}c8\")" "out.m" "生成")
                       ("missing-dir" "(mml G \"c8\")" "missing/out.m" "出力先の不存在")
                       ("directory" "(mml G \"c8\")" "adir" "出力先への rename")
                       ("note-range" "(mml A \"o8c>c\")" "out.m" "音高の範囲外")
                       ("size-limit" "(mml A \"c%20000000\")" "out.m" "大きさの上限"))]
          (let* ([dir (work-file (car case))]
                 [source (build-path dir "input.sxl")])
            (make-directory* (build-path dir "adir"))
            (with-output-to-file source (^[] (display (cadr case))))
            (with-output-to-file (build-path dir "out.m") (^[] (display "既存の生成物")))
            (let* ([before (snapshot dir)]
                   [result (run-sxlc source "-m" "pmd-binary"
                                     "-o" (build-path dir (caddr case)))])
              (test* #"~(cadddr case)の失敗は既存ファイルとディレクトリの内容を変えない"
                     '(1 #t)
                     (list (car result) (equal? before (snapshot dir)))))))
        ;; PMD の音高の 1 バイトで表せない音高は、範囲外の音高と許される範囲を示して失敗する。
        (dolist [case '(("上側" "o8c>c" "o9c")
                       ("上側" "o8b>e-" "o9e-")
                       ("下側" "o1c<c" "o0c")
                       ("下側" "o1c<<b" "o-1b")
                       ("ポルタメントの終了音" "o8/{b >c}8" "o9c"))]
          (let* ([source (write-source (work-file #"note-range-~(caddr case).sxl")
                                       `((mml A ,(cadr case))))]
                 [output (work-file #"note-range-~(caddr case).m")]
                 [result (run-sxlc source "-m" "pmd-binary" "-o" output)])
            (test* #"音高が~(car case)の範囲外なら失敗する: ~(cadr case)"
                   '(1 #t #f)
                   (list (car result)
                         (boolean (any (string->regexp
                                        (regexp-quote #"note out of range: ~(caddr case) (o1c - o8b)"))
                                       (cadr result)))
                         (file-exists? output)))))
        ;; 生成物は 65535 バイトまで、PMD4 の合計 tick は 65535 までとし、超えたら失敗する。
        ;; PMD3 の生成物は 2 * 音符の数 + 41 バイトなので、32747 個で上限になる。
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
                           ((define-metadata memo (,(make-string 70000 #\a))) (mml A "c8"))
                           "PMD data exceeds the size limit (65535 bytes) in metadata")
                          ;; memo の文字列は約 64000 バイトで上限に収まり、位置の表で超える
                          ("メタ情報の位置の表が上限を超える" "pmd-binary" "metadata-table"
                           ((define-metadata memo ,(make-list 1000 (make-string 63 #\a))) (mml A "c8"))
                           "PMD data exceeds the size limit (65535 bytes) in metadata")
                          ;; 音色表がなければ 65523 バイトに収まる
                          ("音色表が上限を超える" "pmd3-binary" "voice-table"
                           (,zero-voice (mml A ,(string-append "@0" (repeat-mml 32740 "c8"))))
                           "PMD data exceeds the size limit (65535 bytes) in voice table")
                          ("65535 バイトの生成物は成功する" "pmd3-binary" "size-max"
                           ((mml A ,(repeat-mml 32747 "c8")))
                           65535)
                          ("65536 バイトの生成物は失敗する" "pmd3-binary" "size-over"
                           ((mml A ,(string-append ")" (repeat-mml 32747 "c8"))))
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
                          ("PMD3 は合計 tick を書かないので 67200 tick も成功する" "pmd3-binary" "ticks-pmd3"
                           ((mml A "c%67200"))
                           #t))]
            (let* ([source (write-source (work-file #"limit-~(caddr case).sxl") (cadddr case))]
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
        (let* ([source (write-source (work-file "post-failure.sxl") '((mml G "c8")))]
               [result (run-sxlc source "-m" "pmd-binary" "-c" "false")])
          (test* "後処理の失敗は終了値 3 となり生成物は残る"
                 '(3 #t)
                 (list (car result) (file-exists? (work-file "post-failure.m"))))))
    (remove-directory* workdir)))

;; If you don't want `gosh' to exit with nonzero status even if
;; the test fails, pass #f to :exit-on-failure.
(test-end :exit-on-failure #t)
