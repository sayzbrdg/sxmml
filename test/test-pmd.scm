;;;
;;; pmd-sourceの検査
;;;

(use gauche.test)

(test-start "pmd-source")
(use sxmml)
(use sxmml.form)
(use sxmml.backend.pmd)
(use gauche.process)
(use file.util)
(use test.helpers)

(let* ([sxlc-command (make-test-sxlc-command)]
       [workdir (sys-mkdtemp (build-path (temporary-directory) "sxmml-test"))])
  ;; name.sxl に forms を書き出し、options を付けて sxlc を実行して出力行を返す
  (define (run-sxlc name forms . options)
    (let1 source (build-path workdir #"~|name|.sxl")
      (with-output-to-file source (^[] (for-each write forms)))
      (process-output->string-list
       `(,@sxlc-command ,@options ,source)
       :error :merge)))
  (define (work-file name) (build-path workdir name))
  ;; 音色 id を定義する式。音色の値で出力を区別できるよう AL に al を使う
  (define (voice-form id al)
    `(define-opn-voice ,id (,al 0) ,@(make-list 4 '(0 0 0 0 0 0 0 0 0))))
  ;; name.sxl の変換結果の生成物の内容を返す
  (define (generated-text name forms)
    (run-sxlc name forms "-m" "pmd")
    (file->string (work-file #"~|name|.mml")))
  ;; CLI を通さず、指定したモードと最適化設定で生成物を返す
  (define (pmd-text name forms mode optimize)
    (let ([source (source-output name (with-output-to-string
                                      (^[] (for-each write forms))))]
          [output (work-file #"~|name|.mml")])
      (parameterize ([sxmml-environment (make <sxmml-environment>)]
                     [sxmml-source-file source])
        (mode)
        (slot-set! (sxmml-environment) 'optimize optimize)
        (load source)
        (slot-set! (sxmml-environment) 'output output)
        ((slot-ref (sxmml-environment) 'backend))
        (file->string output :encoding "cp932"))))
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
  ;; forms をこのプロセスでモード mode の PMD 向けに変換し、成功すればメタデータの行
  ;; (PMD3 ではコメントの行) を、失敗すればエラーのメッセージを返す
  (define (pmd-meta-result name mode forms)
    (let1 source (work-file #"pmd-meta-~|name|.sxl")
      (with-output-to-file source (^[] (for-each write forms)))
      (parameterize ([sxmml-environment (make <sxmml-environment>)]
                     [sxmml-source-file source])
        (guard (e [else (condition-message e)])
          (mode)
          (load source)
          ((slot-ref (sxmml-environment) 'backend))
          (filter #/^(; )?#[a-z]/
                  (string-split (file->string (slot-ref (sxmml-environment) 'output)
                                              :encoding "shift_jis")
                                "\r\n"))))))
  ;; forms をこのプロセスで PMD 向けに変換し、成功すればトラック G の行を、
  ;; 失敗すればエラーのメッセージを返す
  (define (pmd-g-result name forms)
    (let1 source (work-file #"pmd-~|name|.sxl")
      (with-output-to-file source (^[] (for-each write forms)))
      (parameterize ([sxmml-environment (make <sxmml-environment>)]
                     [sxmml-source-file source])
        (guard (e [else (condition-message e)])
          (pmd-source)
          (load source)
          ((slot-ref (sxmml-environment) 'backend))
          (find #/^G / (string-split (file->string (slot-ref (sxmml-environment) 'output))
                                     "\r\n"))))))
  ;; forms をこのプロセスで PMD 向けに変換し、成功すればリズムのパターン R0 の行を、
  ;; 失敗すればエラーのメッセージを返す
  (define (pmd-r0-result name forms)
    (let1 source (work-file #"pmd-~|name|.sxl")
      (with-output-to-file source (^[] (for-each write forms)))
      (parameterize ([sxmml-environment (make <sxmml-environment>)]
                     [sxmml-source-file source])
        (guard (e [else (condition-message e)])
          (pmd-source)
          (load source)
          ((slot-ref (sxmml-environment) 'backend))
          (find #/^R0 / (string-split (file->string (slot-ref (sxmml-environment) 'output)
                                                    :encoding "shift_jis")
                                      "\r\n"))))))
  ;; パーサーを通さず、ループ開始が2個のIRをバックエンドへ渡す。
  (define (multiple-loop-output track output)
    (define (event instrument)
      (if (eq? track 'L)
        (make <sxmml-rhythm-emit> :value #t :ticks 12 instrument #t)
        (make <sxmml-mml-rest> :ticks 12)))
    (parameterize ([sxmml-environment (make <sxmml-environment>)]
                   [sxmml-source-file (work-file "direct-loop.sxl")])
      (pmd-source)
      (slot-set! (sxmml-environment) 'output output)
      (slot-set! (sxmml-environment) 'optimize #f)
      (sxmml-ir-register! (list (event :bass) (make <sxmml-mml-loop>)
                                (event :snare) (make <sxmml-mml-loop>)
                                (event :hihat)) track)
      (sxmml-pmd-source-output)))
  (unwind-protect
      (begin
        (test* "通常トラックの直接IR入力で2個目のLを拒否する"
               (test-error <error> "loop command can be used only once in a track")
               (multiple-loop-output 'G (work-file "direct-loop-G.mml")))
        (let* ([dir (work-file "direct-loop-L")]
               [output (build-path dir "out.mml")]
               [snapshot (^[] (map (^[name] (cons name (file->bytevector (build-path dir name))))
                                   (sort (directory-list dir :children? #t) string<?)))])
          (make-directory* dir)
          (with-output-to-file output (^[] (display "既存の生成物")))
          (let* ([before (snapshot)]
                 [message (guard (e [(is-a? e <error>) (condition-message e)])
                            (multiple-loop-output 'L output)
                            'accepted)])
            (test* "リズムの直接IR入力の重複で失敗し、既存出力を保ち一時ファイルを残さない"
                   '("loop command can be used only once in a track" #t)
                   (list message (equal? before (snapshot))))))
        (test* "Lが1個のリズムのR0・R1・Kを維持する"
               '("R0 \\br%12" "R1 \\sr%12" "K l%12 R0 L R1")
               (filter #/^(R[01]|K) /
                       (string-split (generated-text "single-rhythm-loop"
                                                     '((rhythm L "@b8L@s8"))) "\r\n")))
        ;; 生成物の再現性 (#16)
        ;; 音色定義は番号順に出力し、同じ入力からはバイト単位で同じ生成物が得られること
        (test* "voice definitions are output in number order"
               '(0 1 2 3 4 5 6 7 8 9)
               (map (^m (string->number (m 1)))
                    (filter-map #/^@(\d+)/
                                (string-split (generated-text
                                               "voice-order"
                                               (append (map voice-form '(j i h g f e d c b a) (iota 10))
                                                       '((mml A "@{j}c8@{i}c8@{h}c8@{g}c8@{f}c8@{e}c8@{d}c8@{c}c8@{b}c8@{a}c8"))))
                                              "\r\n"))))
        ;; begin 内で定義しても、同じ入力からは同じ出力になること
        (let1 forms (append (map voice-form '(c b a) '(0 1 2))
                            `((begin ,(voice-form 'e 3) ,(voice-form 'd 4))
                              (mml A "@{a}c8@{e}c8")))
          (test* "the same input yields byte-identical output"
                 (generated-text "reproducible" forms)
                 (generated-text "reproducible" forms)))
        ;; 音色番号は、PMD のバックエンドが生成時に、定義した全ての音色へ id の辞書順で
        ;; 0 から振る。begin の中で定義しても重複せず、257 個以上は失敗する (#27)
        (let ([voice-lines (^[text] (filter-map (^l (and-let1 m (#/^@(\d{3}) (\d{3}) / l)
                                                     (list (string->number (m 1))
                                                           (string->number (m 2)))))
                                               (string-split text "\r\n")))]
              [a-line (^[text] (find #/^A / (string-split text "\r\n")))]
              [many-voices (^[n] (map (^i (voice-form (string->symbol (format "v~3,'0d" i)) 0))
                                      (iota n)))])
          (let1 text (generated-text "voice-dictionary-order"
                                     `(,(voice-form 2 3) ,(voice-form 10 2) ,(voice-form 0 1)
                                       (mml A "@2c8")))
            (test* "voice numbers follow the dictionary order of ids"
                   '("A l%12@2o4c" ((2 3)))
                   (list (a-line text) (voice-lines text))))
          (let1 forms (append (map voice-form '(a b c) '(0 1 2))
                              '((mml A "@{b}@{c}c8")))
            (let1 text (pmd-text "used-voices-O0" forms pmd-source #f)
              (test* "最適化なしでは生成する全音色指定の定義だけを出力する"
                     '("A l%12@1@2o4c" ((1 1) (2 2)))
                     (list (a-line text) (voice-lines text))))
            (let1 text (pmd-text "used-voices-O1" forms pmd-source #t)
              (test* "最適化で消えた音色指定の定義を省き番号を維持する"
                     '("A l%12@2o4c" ((2 2)))
                     (list (a-line text) (voice-lines text)))))
          (let1 text (pmd-text "used-voices-ssg-pcm"
                              `(,(voice-form 0 0) (mml G "@0c8") (mml J "@0c8"))
                              pmd-source #t)
            (test* "SSGとPCMの音色指定ではFM音色定義を出力しない"
                   '(() ("G l%12@0o4c" "J l%12@0o4c"))
                   (list (voice-lines text)
                         (filter #/^[GJ] / (string-split text "\r\n")))))
          (let1 text (pmd-text "used-voices-tracks"
                              (append (map voice-form '(a b c) '(0 1 2))
                                      '((mml A "@{c}c8")
                                        (mml F "@{a}c8@{c}c8")
                                        (mml G "c8") (mml J "c8")
                                        (rhythm L "@b8")))
                              pmd-source #t)
            (test* "複数トラックの使用音色を集約し定義と全本文を順に出力する"
                   '("@000 000 000" "@002 002 000"
                     "A l%12@2o4c" "F l%12@0o4c@2c"
                     "G l%12o4c" "J l%12o4c"
                     "R0 \\br%12" "K l%12 R0")
                   (filter #/^(@\d|[A-JK] |R[01] )/
                           (string-split text "\r\n"))))
          (let1 text (pmd-text "used-voices-pmd3"
                              `(,(voice-form 'a 0) ,(voice-form 'b 1)
                                (mml A "@{b}c8"))
                              pmd3-source #t)
            (test* "PMD3でも未使用定義を省きヘッダと終端を維持する"
                   '(((1 1)) #t "A l%12@1o4c" 26)
                   (list (voice-lines text)
                         (boolean (string-scan text ";; mode: PMDver3.3\r\n"))
                         (a-line text)
                         (char->integer (string-ref text (- (string-length text) 1))))))
          (let1 text (generated-text "voice-in-begin"
                                     `((begin ,(voice-form 'b 1) ,(voice-form 'a 2))
                                       (mml A "@{a}c8@{b}c8")))
            (test* "voices defined in begin get distinct numbers"
                   '("A l%12@0o4c@1c" ((0 2) (1 1)))
                   (list (a-line text) (voice-lines text))))
          (dolist [case '((256 0 #t) (257 1 #f))]
            (let* ([n (car case)]
                   [output (work-file #"voices-~|n|.mml")]
                   [source (source-output #"voices-~n"
                                         (with-output-to-string
                                           (^[] (for-each write (append (many-voices n)
                                                                        '((mml A "@{v000}c8")))))))]
                   [result (sxlc-status "-m" "pmd" "-o" output source)])
              (test* #"~n voices ~(if (zero? (cadr case)) \"are accepted\" \"fail\")"
                     (list (cadr case) (not (caddr case)) (caddr case))
                     (list (car result)
                           (boolean (any #/too many voices: 257 \(max 256\)/ (cadr result)))
                           (file-exists? output))))))
        ;; LFO・エンベロープは、使うときに種類・値の数・値を検査し、何が悪いかを示して
        ;; 失敗する。種類は LFO が default・triangle、エンベロープが default (#28)
        (let ([lfo (^[def channel] `(,def (mml G ,#"M{L1 ~channel}c8")))]
              [envelope (^[def] `(,def (mml G "E{E1}c8")))])
          (dolist [case `(("lfo-type" ,(lfo '(define-lfo L1 sw (1 2 3 4)) "A")
                           "unsupported lfo type: sw in L1 (supported: default, triangle)")
                          ("envelope-type" ,(envelope '(define-envelope E1 sw (1 2 3 4)))
                           "unsupported envelope type: sw in E1 (supported: default)")
                          ("unused-type" ((define-lfo L2 sw (1 2 3 4)) (mml G "c8"))
                           "G l%12o4c")
                          ("lfo-count" ,(lfo '(define-lfo L1 (1 2 3)) "A")
                           "invalid lfo values: L1 has 3 values (expected 4)")
                          ("lfo-count-1" ,(lfo '(define-lfo L1 (1)) "A")
                           "invalid lfo values: L1 has 1 value (expected 4)")
                          ("envelope-count-3" ,(envelope '(define-envelope E1 (1 2 3)))
                           "invalid envelope values: E1 has 3 values (expected 4, 5 or 6)")
                          ("envelope-count-7" ,(envelope '(define-envelope E1 (1 2 3 4 5 6 7)))
                           "invalid envelope values: E1 has 7 values (expected 4, 5 or 6)")
                          ("envelope-5" ,(envelope '(define-envelope E1 (1 2 3 4 5)))
                           "G l%12E1,2,3,4,5o4c")
                          ("lfo-inexact" ,(lfo '(define-lfo L1 (1 1.5 3 4)) "A")
                           "invalid lfo value: L1, value 2 is 1.5 (0 - 255)")
                          ("lfo-string" ,(lfo '(define-lfo L1 (1 2 "x" 4)) "A")
                           "invalid lfo value: L1, value 3 is \"x\" (-128 - 127)")
                          ("lfo-list" ,(lfo '(define-lfo L1 (1 2 3 (list 1 2))) "A")
                           "invalid lfo value: L1, value 4 is (1 2) (0 - 255)")
                          ("lfo-range" ,(lfo '(define-lfo L1 (1 256 3 4)) "A")
                           "invalid lfo value: L1, value 2 is 256 (0 - 255)")
                          ("envelope-range-1" ,(envelope '(define-envelope E1 (1 16 3 4)))
                           "invalid envelope value: E1, value 2 is 16 (-15 - 15)")
                          ("envelope-range-2" ,(envelope '(define-envelope E1 (32 2 3 4 5)))
                           "invalid envelope value: E1, value 1 is 32 (0 - 31)")
                          ("lfo-bounds" ,(lfo '(define-lfo L1 (0 255 -128 127)) "A")
                           "G l%12M0,255,-128,127o4c")
                          ;; 各値の範囲の両端は通る
                          ("lfo-min" ,(lfo '(define-lfo L1 (0 0 -128 0)) "A")
                           "G l%12M0,0,-128,0o4c")
                          ("lfo-max" ,(lfo '(define-lfo L1 (255 255 127 255)) "A")
                           "G l%12M255,255,127,255o4c")
                          ("envelope-4-min" ,(envelope '(define-envelope E1 (0 -15 0 0)))
                           "G l%12E0,-15,0,0o4c")
                          ("envelope-4-max" ,(envelope '(define-envelope E1 (255 15 255 255)))
                           "G l%12E255,15,255,255o4c")
                          ("envelope-5-min" ,(envelope '(define-envelope E1 (0 0 0 0 0)))
                           "G l%12E0,0,0,0,0o4c")
                          ("envelope-5-max" ,(envelope '(define-envelope E1 (31 31 31 15 15)))
                           "G l%12E31,31,31,15,15o4c")
                          ("envelope-6-min" ,(envelope '(define-envelope E1 (0 0 0 0 0 0)))
                           "G l%12E0,0,0,0,0,0o4c")
                          ("envelope-6-max" ,(envelope '(define-envelope E1 (31 31 31 15 15 15)))
                           "G l%12E31,31,31,15,15,15o4c")
                          ("lfo-channel" ,(lfo '(define-lfo L1 (1 2 3 4)) "C")
                           "invalid lfo channel: C")
                          ;; LFO の切り替えのチャンネルの誤りも、定義と同じメッセージにする
                          ("lfo-switch-channel" ((mml G "Mt{C}c8"))
                           "invalid lfo channel: C"))]
            (test* #"PMD checks LFO and envelope definitions: ~(car case)"
                   (caddr case)
                   (pmd-g-result (car case) (cadr case))))
          (test* "a triangle LFO is output the same as the default"
                 (pmd-g-result "lfo-default" (lfo '(define-lfo L1 (1 2 3 4)) "A"))
                 (pmd-g-result "lfo-triangle" (lfo '(define-lfo L1 triangle (1 2 3 4)) "A"))))
        ;; PMD が使うメタデータは、title などが文字列、memo が文字列か文字列のリストで、
        ;; それ以外は書くときにキーと値を示して失敗する (#13)
        (dolist [case `(("title-number" ,pmd-source ((define-metadata title 1))
                         "invalid metadata value: title is 1 (expected a string)")
                        ("title-list" ,pmd-source ((define-metadata title (list "t1" "t2")))
                         "invalid metadata value: title is (\"t1\" \"t2\") (expected a string)")
                        ("memo-element" ,pmd-source ((define-metadata memo (list "a" 1)))
                         "invalid metadata value: memo is (\"a\" 1) (expected a string or a list of strings)")
                        ("memo-number" ,pmd-source ((define-metadata memo 1))
                         "invalid metadata value: memo is 1 (expected a string or a list of strings)")
                        ("memo-empty" ,pmd-source ((define-metadata memo '())) ())
                        ("strings" ,pmd-source ((define-metadata title "t") (define-metadata composer "c"))
                         ("#title       t" "#composer    c"))
                        ("pmd3-title-number" ,pmd3-source ((define-metadata title 1))
                         "invalid metadata value: title is 1 (expected a string)"))]
          (test* #"PMD checks metadata values: ~(car case)"
                 (cadddr case)
                 (pmd-meta-result (car case) (cadr case) `(,@(caddr case) (mml G "c8")))))
        ;; memo は文字列 1 つなら 1 行、リストなら要素ごとに 1 行で出力すること (#87)
        (dolist [case '(("memo-string" "abc" ("#memo        abc"))
                        ("memo-list" (list "m1" "m2") ("#memo        m1" "#memo        m2")))]
          (test* #"memo is output as written: ~(car case)"
                 (caddr case)
                 (filter #/^#memo/ (string-split (generated-text (car case)
                                                                 `((define-metadata memo ,(cadr case))
                                                                   (mml G "c8")))
                                                 "\r\n"))))
        ;; エンベロープは値の数で書式が決まる。5 値は 6 値の書式の 6 つ目を省いた形なので、
        ;; 5 値のまま出力すること (#84)
        (test* "a 5-value envelope is output with its 5 values"
               "G l%12E1,2,3,4o4cE1,2,3,4,5cE1,2,3,4,5,6c"
               (find #/^G / (string-split (generated-text "envelope-values"
                                                          '((define-envelope E4 (1 2 3 4))
                                                            (define-envelope E5 (1 2 3 4 5))
                                                            (define-envelope E6 (1 2 3 4 5 6))
                                                            (mml G "E{E4}c8E{E5}c8E{E6}c8")))
                                          "\r\n")))
        ;; 負の音高も、オクターブと 0～11 の音名に分けて出力すること
        (test* "a negative note number is output with its octave and note name"
               "A l%24o1co-1b"
               (find #/^A / (string-split (generated-text "negative-note" '((mml A "o1c<<b")))
                                          "\r\n")))
        ;; リズムのパンは 1（右）・2（左）・3（中央）で、範囲外は値と範囲を示して失敗する。
        ;; これまでは \#<undef>b のような壊れた MML を書いていた (#102)
        (test* "rhythm pans 1, 2 and 3 are output as right, left and center"
               "R0 \\rb\\br%12\\lb\\br%12\\mb\\br%12"
               (pmd-r0-result "rhythm-pan" '((rhythm L "pb1@b8pb2@b8pb3@b8"))))
        (dolist [value '(0 4)]
          (test* #"a rhythm pan out of range fails: ~value"
                 #"rhythm pan out of range: ~value (1 - 3)"
                 (pmd-r0-result #"rhythm-pan-~value" `((rhythm L ,#"pb~|value|@b8")))))
        ;; 楽器別の音量は、他の命令と同じく値をそのまま書く。検査の範囲は #110 で決める
        (test* "a rhythm instrument volume is output as it is"
               "R0 \\vb32\\br%12"
               (pmd-r0-result "rhythm-volume" '((rhythm L "vb32@b8"))))
        ;; PCM の音色は 0 から 255、SSG の音色は 0 から 9 までの整数とし、
        ;; それ以外は書かれた値を示して失敗する (#58)
        (test* "a PCM voice 0 is accepted"
               0
               (car (sxlc-status "-m" "pmd" "-o" (work-file "voice-pcm-0.mml")
                                 (source-output "voice-pcm-0" "(mml J \"@0c8\")"))))
        (dolist (case '(("J" "256") ("J" "1.5") ("J" "x") ("G" "10") ("G" "1.5") ("G" "x")))
          (let1 source (source-output #"voice-~(car case)-~(cadr case)"
                                     #"(mml ~(car case) \"@{~(cadr case)}c8\")")
            (test* #"a voice ~(cadr case) on track ~(car case) fails"
                   '(1 #t)
                   (status-and-message (sxlc-status "-m" "pmd" "-o" (work-file "voice-invalid.mml") source)
                                       (string->regexp
                                        #"invalid voice parameter: +~(regexp-quote (cadr case))$")))))
      )
    (remove-directory* workdir)))

(test-end :exit-on-failure #t)
