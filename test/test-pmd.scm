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
  ;; 値と反復の検査は、パーサーを通さず公開バックエンドへ IR を渡す
  (define (pmd-ir-text name track ir :optional (setup (^[] #f)))
    (parameterize ([sxmml-environment (make <sxmml-environment>)]
                   [sxmml-source-file (work-file #"~|name|.sxl")])
      (pmd-source)
      (slot-set! (sxmml-environment) 'optimize #f)
      (slot-set! (sxmml-environment) 'output (work-file #"~|name|.mml"))
      (setup)
      (sxmml-ir-register! ir track)
      (sxmml-pmd-source-output)
      (file->string (work-file #"~|name|.mml") :encoding "cp932")))
  (define (pmd-ir-result name track ir :optional (setup (^[] #f)))
    (guard (e [else (condition-message e)])
      (filter #/^(@\d|[A-JK] |R[01] )/
              (string-split (pmd-ir-text name track ir setup) "\r\n"))))
  (define (range-message kind value low high)
    (format "invalid ~a: ~s (expected an integer from ~a to ~a)" kind value low high))
  (define (range-ir obj track)
    (list obj (if (eq? track 'L)
                (make <sxmml-rhythm-emit> :value #t :bass #t :ticks 12)
                (make <sxmml-mml-note> :value 48 :ticks 12))))
  (define (repeat-ir depth count track)
    (append (make-list depth (make <sxmml-mml-repeat-start>))
            (cdr (range-ir #f track))
            (make-list depth (make <sxmml-mml-repeat-end> :value count))))
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
        (test* "未定義LFOを名前付きの参照エラーで通知する"
               (test-error <sxmml-reference-error> "undefined LFO: UNKNOWN")
               (pmd-text "undefined-lfo" '((mml A "M{UNKNOWN A}c8")) pmd-source #f))
        (test* "未定義envelopeを名前付きの参照エラーで通知する"
               (test-error <sxmml-reference-error> "undefined envelope: UNKNOWN")
               (pmd-text "undefined-envelope" '((mml G "E{UNKNOWN}c8")) pmd-source #f))

        (test* "公開pmd-source出力の範囲違反は値エラーになる"
               (test-error <sxmml-value-error> #/invalid FM volume: 128/)
               (pmd-ir-text "typed-volume" 'A
                            (range-ir (make <sxmml-mml-volume> :value 128) 'A)))
        (test* "公開pmd-source出力の未定義音色は参照エラーになる"
               (test-error <sxmml-reference-error> #/undefined voice/)
               (pmd-ir-text "typed-reference" 'A
                            (range-ir (make <sxmml-mml-voice> :value 'UNKNOWN) 'A)))
        (test* "音色数の上限違反は基底sxmml-errorになる"
               #t
               (parameterize ([sxmml-environment (make <sxmml-environment>)])
                 (dolist [n (iota 257)]
                   (apply sxmml-opn-voice-set! (string->symbol #"V~n") '(0 0)
                          (make-list 4 '(0 0 0 0 0 0 0 0 0))))
                 (guard (e [else (eq? (class-of e) <sxmml-error>)])
                   (sxmml-pmd-voice-number-table-compute))))

        (test* "通常トラックの直接IR入力で2個目のLを拒否する"
               (test-error <sxmml-mml-syntax-error> "loop command can be used only once in a track")
               (multiple-loop-output 'G (work-file "direct-loop-G.mml")))
        (let* ([dir (work-file "direct-loop-L")]
               [output (build-path dir "out.mml")]
               [snapshot (^[] (map (^[name] (cons name (file->bytevector (build-path dir name))))
                                   (sort (directory-list dir :children? #t) string<?)))])
          (make-directory* dir)
          (with-output-to-file output (^[] (display "既存の生成物")))
          (let* ([before (snapshot)]
                 [message (guard (e [(condition-has-type? e <error>) (condition-message e)])
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
                                               (append (map voice-form '(j i h g f e d c b a) (make-list 10 0))
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
                           "unsupported LFO type: sw in L1 (supported: default, triangle)")
                          ("envelope-type" ,(envelope '(define-envelope E1 sw (1 2 3 4)))
                           "unsupported envelope type: sw in E1 (supported: default)")
                          ("unused-type" ((define-lfo L2 sw (1 2 3 4)) (mml G "c8"))
                           "G l%12o4c")
                          ("lfo-count" ,(lfo '(define-lfo L1 (1 2 3)) "A")
                           "invalid LFO values: L1 has 3 values (expected 4)")
                          ("lfo-count-1" ,(lfo '(define-lfo L1 (1)) "A")
                           "invalid LFO values: L1 has 1 value (expected 4)")
                          ("envelope-count-3" ,(envelope '(define-envelope E1 (1 2 3)))
                           "invalid envelope values: E1 has 3 values (expected 4, 5 or 6)")
                          ("envelope-count-7" ,(envelope '(define-envelope E1 (1 2 3 4 5 6 7)))
                           "invalid envelope values: E1 has 7 values (expected 4, 5 or 6)")
                          ("envelope-5" ,(envelope '(define-envelope E1 (1 2 3 4 5)))
                           "G l%12E1,2,3,4,5o4c")
                          ("lfo-inexact" ,(lfo '(define-lfo L1 (1 1.5 3 4)) "A")
                           "invalid LFO L1 value 2: 1.5 (expected an integer from 0 to 255)")
                          ("lfo-string" ,(lfo '(define-lfo L1 (1 2 "x" 4)) "A")
                           "invalid LFO L1 value 3: \"x\" (expected an integer from -128 to 127)")
                          ("lfo-list" ,(lfo '(define-lfo L1 (1 2 3 (list 1 2))) "A")
                           "invalid LFO L1 value 4: (1 2) (expected an integer from 0 to 255)")
                          ("lfo-range" ,(lfo '(define-lfo L1 (1 256 3 4)) "A")
                           "invalid LFO L1 value 2: 256 (expected an integer from 0 to 255)")
                          ("envelope-range-1" ,(envelope '(define-envelope E1 (1 16 3 4)))
                           "invalid envelope E1 value 2: 16 (expected an integer from -15 to 15)")
                          ("envelope-range-2" ,(envelope '(define-envelope E1 (32 2 3 4 5)))
                           "invalid envelope E1 value 1: 32 (expected an integer from 0 to 31)")
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
                           "invalid LFO channel: C")
                          ;; LFO の切り替えのチャンネルの誤りも、定義と同じメッセージにする
                          ("lfo-switch-channel" ((mml G "Mt{C}c8"))
                           "invalid LFO channel: C"))]
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
        ;; 負の音高は、値と許容範囲を示して失敗すること
        (test* "負の音高は終了値1と範囲外の診断を返す"
               '(1 #t)
               (status-and-message
                (sxlc-status "-m" "pmd" "-o" (work-file "negative-note.mml")
                             (source-output "negative-note" "(mml A \"o1c<<b\")"))
                (string->regexp
                 (regexp-quote "invalid note: o-1b (expected o1c - o8b)"))))
        ;; リズムのパンは 1（右）・2（左）・3（中央）で、範囲外は値と範囲を示して失敗する。
        ;; これまでは \#<undef>b のような壊れた MML を書いていた (#102)
        (test* "rhythm pans 1, 2 and 3 are output as right, left and center"
               "R0 \\rb\\br%12\\lb\\br%12\\mb\\br%12"
               (pmd-r0-result "rhythm-pan" '((rhythm L "pb1@b8pb2@b8pb3@b8"))))
        (dolist [value '(0 4)]
          (test* #"a rhythm pan out of range fails: ~value"
                 #"invalid rhythm pan: ~value (expected an integer from 1 to 3)"
                 (pmd-r0-result #"rhythm-pan-~value" `((rhythm L ,#"pb~|value|@b8")))))
        ;; 楽器別の音量は有効範囲の値をそのまま出力する
        (test* "a rhythm instrument volume within range is output"
               "R0 \\vb31\\br%12"
               (pmd-r0-result "rhythm-volume" '((rhythm L "vb31@b8"))))
        (for-each
         (^[case]
           (apply (^[name track class accepted rejected kind low high line]
                    (test* #"~|name|の境界値を出力する"
                           (list line)
                           (pmd-ir-result #"range-~|kind|-ok" track
                                          (range-ir (make class :value accepted) track)))
                    (test* #"~|name|の範囲外を拒否する"
                           (range-message kind rejected low high)
                           (pmd-ir-result #"range-~|kind|-bad" track
                                          (range-ir (make class :value rejected) track))))
                  case))
         `(("FM音量" A ,<sxmml-mml-volume> 127 128 "FM volume" 0 127 "A V127o4c%12")
           ("SSG音量" G ,<sxmml-mml-volume> 15 16 "SSG volume" 0 15 "G v15o4c%12")
           ("PCM音量" J ,<sxmml-mml-volume> 255 256 "PCM volume" 0 255 "J V255o4c%12")
           ("ゲートタイム" A ,<sxmml-mml-gate-time> 255 256 "gate time" 0 255 "A q255o4c%12")
           ("デチューン" A ,<sxmml-mml-detune> -32768 -32769 "detune" -32768 32767 "A D-32768o4c%12")
           ("絶対転調" A ,<sxmml-mml-transpose> 127 128 "transpose" -128 127 "A _127o4c%12")
           ("相対転調" A ,<sxmml-mml-transpose-change> -128 -129 "relative transpose" -128 127 "A __-128o4c%12")
           ("相対音量" A ,<sxmml-mml-volume-change> 255 256 "relative volume" -255 255 "A )%255o4c%12")
           ("パン" A ,<sxmml-mml-pan> 3 4 "pan" 0 3 "A p3o4c%12")
           ("出力選択" G ,<sxmml-mml-output-switch> 1 0 "output switch" 1 3 "G P1o4c%12")
           ("ノイズ周波数" G ,<sxmml-mml-noise-frequency> 31 32 "noise frequency" 0 31 "G w31o4c%12")
           ("Timer-B" A ,<sxmml-mml-tempo-absolute> 255 256 "Timer-B" 0 255 "A T255o4c%12")))
        (test* "SSGのパンもコマンド共通の範囲で出力する"
               '("G p0o4c%12")
               (pmd-ir-result "ssg-pan" 'G (range-ir (make <sxmml-mml-pan> :value 0) 'G)))
        (for-each
         (^[case]
           (apply (^[name class slot accepted rejected kind low high line]
                    (test* #"~|name|の境界値を出力する"
                           (list line "K R0")
                           (pmd-ir-result #"rhythm-~|kind|-ok" 'L
                                          (range-ir (make class slot accepted) 'L)))
                    (test* #"~|name|の範囲外を拒否する"
                           (range-message kind rejected low high)
                           (pmd-ir-result #"rhythm-~|kind|-bad" 'L
                                          (range-ir (make class slot rejected) 'L))))
                  case))
         `(("リズム全体音量" ,<sxmml-rhythm-volume> :value 63 64 "rhythm volume" 0 63 "R0 \\V63\\br%12")
           ("リズム楽器別音量" ,<sxmml-rhythm-volume> :bass 31 32 "rhythm instrument volume" 0 31 "R0 \\vb31\\br%12")
           ("リズム相対音量" ,<sxmml-rhythm-volume-change> :bass -31 -32 "relative rhythm volume" -31 31 "R0 \\vb-31\\br%12")))
        (test* "通常音符の音域上限を出力する"
               '("A o8b%12")
               (pmd-ir-result "note-high" 'A (list (make <sxmml-mml-note> :value 107 :ticks 12))))
        (test* "通常音符の音域外を拒否する"
               "invalid note: o9c (expected o1c - o8b)"
               (pmd-ir-result "note-outside" 'A (list (make <sxmml-mml-note> :value 108 :ticks 12))))
        (let ([upper '(31 31 31 15 15 127 3 15 7 0 0 1)]
              [ir (range-ir (make <sxmml-mml-voice> :value 'tone) 'A)])
          (define (voice-result name alfb op :optional (used? #t))
            (pmd-ir-result name 'A (if used? ir (cdr ir))
                           (^[] (sxmml-opn-voice-set! 'tone alfb op op op op))))
          (test* "FM音色の出力パラメータ上限を受理する"
                 (append '("@000 007 007")
                         (make-list 4 " 031 031 031 015 015 127 003 015 007 001")
                         '("A @0o4c%12"))
                 (guard (e [else (condition-message e)])
                   (filter #/^(@\d| \d|A )/
                           (string-split
                            (pmd-ir-text "voice-upper" 'A ir
                                         (^[] (sxmml-opn-voice-set! 'tone '(7 7) upper upper upper upper)))
                            "\r\n"))))
          (for-each
           (^[case]
             (apply (^[field index value high]
                      (test* #"FM音色~|field|の範囲外を拒否する"
                             (range-message #"FM voice ~field" value 0 high)
                             (voice-result #"voice-~field" (if (= index -1) '(8 7) '(7 7))
                                           (map (^[i v] (if (= i index) value v)) (iota 12) upper))))
                    case))
           '(("AL" -1 8 7) ("AR" 0 32 31) ("RR" 3 16 15) ("TL" 5 128 127)
             ("KS" 6 4 3) ("DT" 8 8 7) ("AMS" 11 2 1)))
          (test* "出力しないFM音色の上限は追加検査しない"
                 '("A o4c%12")
                 (voice-result "voice-unused" '(8 7) upper #f)))
        (test* "長い通常音符の分割出力を維持する"
               '("A o4c%255&c%45")
               (pmd-ir-result "long-note" 'A (list (make <sxmml-mml-note> :value 48 :ticks 300))))
        (test* "反復回数255を出力する"
               '("A [o4c%12]255")
               (pmd-ir-result "repeat-255" 'A (repeat-ir 1 255 'A)))
        (test* "反復回数256を拒否する"
               (range-message "repeat count" 256 1 255)
               (pmd-ir-result "repeat-256" 'A (repeat-ir 1 256 'A)))
        (test* "通常トラックの反復32段を出力する"
               (list (string-append "A " (make-string 32 #\[) "o4c%12"
                                    (apply string-append (make-list 32 "]2"))))
               (pmd-ir-result "repeat-depth-32" 'A (repeat-ir 32 2 'A)))
        (test* "通常トラックの反復33段を拒否する"
               (range-message "repeat depth" 33 1 32)
               (pmd-ir-result "repeat-depth-33" 'A (repeat-ir 33 2 'A)))
        (test* "リズムトラックの反復33段を拒否する"
               (range-message "repeat depth" 33 1 32)
               (pmd-ir-result "rhythm-depth-33" 'L (repeat-ir 33 2 'L)))
        (test* "ポルタメントの音程と音長の上限を出力する"
               '("A o1{co8b}%255")
               (pmd-ir-result "portamento-upper" 'A
                              (list (make <sxmml-mml-portamento> :value '(12 . 107) :ticks 255))))
        (test* "ポルタメントの音長256を拒否する"
               (range-message "portamento ticks" 256 1 255)
               (pmd-ir-result "portamento-long" 'A
                              (list (make <sxmml-mml-portamento> :value '(12 . 107) :ticks 256))))
        (test* "ポルタメントの始点音域外は音名で示す"
               "invalid portamento start note: o0b (expected o1c - o8b)"
               (pmd-ir-result "portamento-start" 'A
                              (list (make <sxmml-mml-portamento> :value '(11 . 48) :ticks 12))))
        (test* "ポルタメントの終点音域外を拒否する"
               "invalid portamento end note: o9c (expected o1c - o8b)"
               (pmd-ir-result "portamento-end" 'A
                              (list (make <sxmml-mml-portamento> :value '(48 . 108) :ticks 12))))
        (test* "整数でないゲートタイムを拒否する"
               (range-message "gate time" 1/2 0 255)
               (pmd-ir-result "gate-fraction" 'A
                              (range-ir (make <sxmml-mml-gate-time> :value 1/2) 'A)))
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
                                        #"invalid ~(if (equal? (car case) \"J\") \"PCM\" \"SSG\") voice number: ~(regexp-quote (cadr case)) \\(expected an integer from 0 to ~(if (equal? (car case) \"J\") 255 9)\\)$")))))
      )
    (remove-directory* workdir)))

(test-end :exit-on-failure #t)
