;;;
;;; pmd-sourceの検査
;;;

(use gauche.test)
(use srfi.13)

(test-start "pmd-source")
(use sxmml)
(use sxmml.form)
(use sxmml.backend.pmd)
(use sxmml.backend.pmd-source)
(test-module 'sxmml.backend.pmd)
(test-module 'sxmml.backend.pmd-source)
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
    (let ([source (source-output name (with-output-to-string (^[] (for-each write forms))))]
          [output (work-file #"~|name|.mml")])
      (parameterize ([sxmml-environment (make <sxmml-environment>)])
        (sxmml-mode-select! mode)
        (load source)
        ((slot-ref (sxmml-mode-get) 'backend)
         (make-sxmml-output-environment (sxmml-environment) :output output
           :optimize (case optimize [(#f) 0] [(#t) 1] [(:full) 2] [(:repeat) 3] [else 4])
           :source source))
        (file->string output :encoding "cp932"))))
  ;; 値と反復の検査は、パーサーを通さず公開バックエンドへ IR を渡す
  (define (pmd-ir-text name track ir :optional (setup (^[] #f)))
    (parameterize ([sxmml-environment (make <sxmml-environment>)])
      (sxmml-mode-select! 'pmd-source)
      (setup)
      (sxmml-ir-register! ir track)
      (let1 result (sxmml-pmd-source-output
                   (make-sxmml-output-environment (sxmml-environment)
                     :output (work-file #"~|name|.mml") :optimize 0
                     :source (work-file #"~|name|.sxl")))
        (file->string (slot-ref result 'output) :encoding "cp932"))))
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
          (sxmml-mode-select! mode)
          (load source)
          ((slot-ref (sxmml-mode-get) 'backend)
           (make-sxmml-output-environment (sxmml-environment)
             :output (path-swap-extension source "mml") :optimize 1 :source source))
          (filter #/^(; )?#[a-z]/
                  (string-split (file->string (path-swap-extension source "mml")
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
          (sxmml-mode-select! 'pmd-source)
          (load source)
          ((slot-ref (sxmml-mode-get) 'backend)
           (make-sxmml-output-environment (sxmml-environment)
             :output (path-swap-extension source "mml") :optimize 1 :source source))
          (find #/^G / (string-split (file->string (path-swap-extension source "mml"))
                                     "\r\n"))))))
  ;; forms をこのプロセスで PMD 向けに変換し、成功すればリズムのパターン R0 の行を、
  ;; 失敗すればエラーのメッセージを返す
  (define (pmd-r0-result name forms)
    (let1 source (work-file #"pmd-~|name|.sxl")
      (with-output-to-file source (^[] (for-each write forms)))
      (parameterize ([sxmml-environment (make <sxmml-environment>)]
                     [sxmml-source-file source])
        (guard (e [else (condition-message e)])
          (sxmml-mode-select! 'pmd-source)
          (load source)
          ((slot-ref (sxmml-mode-get) 'backend)
           (make-sxmml-output-environment (sxmml-environment)
             :output (path-swap-extension source "mml") :optimize 1 :source source))
          (find #/^R0 / (string-split (file->string (path-swap-extension source "mml")
                                                    :encoding "shift_jis")
                                      "\r\n"))))))
  ;; パーサーを通さず、ループ開始が2個のIRをバックエンドへ渡す。
  (define (multiple-loop-output track output)
    (define (event instrument)
      (if (eq? track 'L)
        (make <sxmml-rhythm-emit> :value #t :ticks 12 instrument #t)
        (make <sxmml-mml-rest> :ticks 12)))
    (parameterize ([sxmml-environment (make <sxmml-environment>)])
      (sxmml-mode-select! 'pmd-source)
      (sxmml-ir-register! (list (event :bass) (make <sxmml-mml-loop>)
                                (event :snare) (make <sxmml-mml-loop>) (event :hihat)) track)
      (sxmml-pmd-source-output
       (make-sxmml-output-environment (sxmml-environment) :output output :optimize 0))))

  (parameterize ([sxmml-environment (make <sxmml-environment>)])
    (sxmml-mode-select! 'pmd-source)
    (sxmml-resolution-set! 48)
    (define-metadata title "明示した環境")
    (define-lfo effect (list 1 2 3 4))
    (define-envelope shape (list 1 2 3 4))
    (mml G "M{effect A}E{shape}v8t120c8")
    (rename-track G lead)
    (define-track unused)
    (mml unused "r%70000")
    (let* ([registered (sxmml-environment)]
           [out (make-sxmml-output-environment registered
                  :output (work-file "explicit.mml") :optimize 2)]
           [original (slot-ref (sxmml-track-get 'lead) 'compiled-data)]
           [ctx (sxmml-track-get 'lead)])
      (test* "出力先#fは補完せず値エラーにする"
             (test-error <sxmml-value-error> "output file is not specified")
             (sxmml-pmd-source-output (make-sxmml-output-environment registered :output #f :optimize 0)))
      (parameterize ([sxmml-environment (make <sxmml-environment>)]
                     [sxmml-source-file "wrong-source.sxl"])
        (let* ([result (sxmml-pmd-source-output out)]
               [first (file->bytevector (slot-ref result 'output))])
          (test* "明示した出力環境から生成できる" #t (is-a? result <sxmml-output-result>))
          (test* "採用した名前はpartではなく変更後のトラック名" '(#t #f #f)
                 (let1 names (slot-ref result 'track-names)
                   (map (^[name] (boolean (memq name names))) '(lead G unused))))
          (sxmml-pmd-source-output out)
          (test* "同じ出力環境で同じ生成物を得て、登録IRを変更しない" '(#t #t 24)
                 (list (equal? first (file->bytevector (slot-ref result 'output)))
                       (eq? original (slot-ref ctx 'compiled-data))
                       (slot-ref (find (^[o] (is-a? o <sxmml-mml-note>))
                                       (sxmml-ir-flatten original)) 'ticks)))))))
  (parameterize ([sxmml-environment (make <sxmml-environment>)])
    (sxmml-mode-select! 'pmd3-source)
    (mml G "c8")
    (let1 out (make-sxmml-output-environment (sxmml-environment)
                 :output (work-file "without-source.mml") :optimize 0)
      (sxmml-pmd3-source-output out)
      (test* "入力元なしはfromだけを省き、PMD3の改行と終端を保つ" '(#t #t)
             (let1 text (file->string (slot-ref out 'output) :encoding "cp932")
               (list (string-prefix? ";; This file was generated by sxmml\r\n;; mode: PMDver3.3\r\n" text)
                     (char=? (string-ref text (- (string-length text) 1)) #\x1a))))
      (slot-set! out 'source "/explicit/named-input.sxl")
      (parameterize ([sxmml-source-file "/wrong/other.sxl"])
        (sxmml-pmd3-source-output out))
      (test* "明示した入力元名を使う" #t
             (boolean (string-contains (file->string (slot-ref out 'output) :encoding "cp932")
                              "from named-input.sxl")))))
  (unwind-protect
      (begin
        (let ()
          (define (render mode form)
            (pmd-text "default-length" (list form) mode #f))
          (define diagnostic
            "invalid PMD3 default note length: 5 ticks (expected a divisor of 96 ticks at resolution 24)")
          (test* "PMD3 通常トラックの既定音長は分母で出力する" #t
                 (boolean (string-contains (render 'pmd3 '(mml G "c8")) "G l8o4c")))
          (test* "PMD3 リズムの K 行の既定音長は分母で出力する" #t
                 (boolean (string-contains (render 'pmd3 '(rhythm L "@b8")) "K l8 R0")))
          (test* "PMD3 通常トラックで分母に換算できない音長はエラー"
                 (test-error <sxmml-value-error> diagnostic)
                 (render 'pmd3 '(mml G "c%5")))
          (test* "PMD3 リズムで分母に換算できない音長はエラー"
                 (test-error <sxmml-value-error> diagnostic)
                 (render 'pmd3 '(rhythm L "@b%5")))
          (test* "PMD4 は通常トラックと K 行を tick 表記で出力する" '(#t #t)
                 (let1 text (pmd-text "default-length-pmd4"
                                      '((mml G "c%5") (rhythm L "@b%5")) 'pmd #f)
                   (list (boolean (string-contains text "G l%5o4c"))
                         (boolean (string-contains text "K l%5 R0"))))))
        (let ([settings '((define-metadata detune 'extend) (define-metadata lfospeed 'normal))])
          (define (directives mode)
            (filter #/^(; )?#(Detune|LFOSpeed|EnvelopeSpeed) /
                    (string-split (pmd-text "extended-settings" settings mode #f) "\r\n")))
          (test* "明示した設定だけをPMD4の宣言にする"
                 '("#Detune Extend" "#LFOSpeed Normal") (directives 'pmd))
          (test* "PMD3では曲単位設定をコメントにする"
                 '("; #Detune Extend" "; #LFOSpeed Normal") (directives 'pmd3))
          (test* "拡張設定に明示した偽値は未指定と区別する"
                 (test-error <sxmml-value-error> "invalid metadata value: detune is #f (expected normal or extend)")
                 (pmd-text "bad-setting" '((define-metadata detune #f)) 'pmd #f)))
        (let ()
          (define (lines forms :optional (mode 'pmd))
            (filter #/^(; )?#PPZExtend|^[a-h] /
                    (string-split (pmd-text "ppz-parts" forms mode #f) "\r\n")))
          (define tracks '((define-track z :sound-source 'ppz8)
                           (define-track a :sound-source 'ppz8)
                           (define-track empty :sound-source 'ppz8)))
          (define (many n)
            (append-map (^[i] (let1 id (string->symbol (format "track~d" i))
                                `((define-track ,id :sound-source 'ppz8) (mml ,id "r8"))))
                        (iota n)))
          (test* "空のPPZ8は宣言も本文も出さない" '() (lines tracks))
          (test* "PPZ8は休符も含め名前順に割り当てる"
                 '("#PPZExtend ab" "a l%12r" "b l%12@128o4c")
                 (lines (append tracks '((mml z "@128o4c8") (mml a "r8")))))
          (test* "PPZ8の8パートを使用できる" "#PPZExtend abcdefgh" (car (lines (many 8))))
          (test* "PPZ8の9パート目を拒否する" (test-error <sxmml-value-error>) (lines (many 9)))
          (test* "PMD3ソースではPPZ8宣言のみコメントにする"
                 '("; #PPZExtend a" "a l8r")
                 (lines (append tracks '((mml a "r8"))) 'pmd3)))

        (parameterize ([sxmml-environment (make <sxmml-environment>)])
          (define (render jobs)
            (let1 path (work-file "jobs-output")
              (sxmml-pmd-source-output
               (make-sxmml-output-environment (sxmml-environment) :output path :optimize 3 :jobs jobs))
              (file->bytevector path)))
          (sxmml-pmd-setup!)
          (sxmml-track-delete! 'B)
          (mml A "cdecdecdecd")
          (mml G "[fg]4")
          (test* "反復圧縮の逐次・並列で生成物が一致" #t (equal? (render 1) (render 2))))
        (test* "未定義LFOを名前付きの参照エラーで通知する"
               (test-error <sxmml-reference-error> "undefined LFO: UNKNOWN")
               (pmd-text "undefined-lfo" '((mml A "M{UNKNOWN A}c8")) 'pmd-source #f))
        (test* "未定義envelopeを名前付きの参照エラーで通知する"
               (test-error <sxmml-reference-error> "undefined envelope: UNKNOWN")
               (pmd-text "undefined-envelope" '((mml G "E{UNKNOWN}c8")) 'pmd-source #f))

        (test* "公開pmd-source出力の範囲違反は値エラーになる"
               (test-error <sxmml-value-error> #/invalid FM volume: 128/)
               (pmd-ir-text "typed-volume" 'A
                            (range-ir (make <sxmml-mml-volume> :value 128) 'A)))
        (parameterize ([sxmml-environment (make <sxmml-environment>)])
          (define (generate)
            (sxmml-pmd-source-output
             (make-sxmml-output-environment (sxmml-environment) :output (work-file "sources.mml") :optimize 0)))
          (sxmml-pmd-setup!)
          (define-track extra :sound-source 'opm)
          (test* "固定パート外の未対応音源も警告する"
                 "warning: pmd-source: ignoring track extra (unsupported sound-source: opm)\n"
                 (let1 err (open-output-string)
                   (parameterize ([current-error-port err]) (generate))
                   (get-output-string err)))
          (sxmml-track-delete! 'extra)
          (define-track A :sound-source 'opm)
          (define-track lead :part 'A :sound-source 'opn)
          (mml lead "c8")
          (test* "未対応音源を実効パートの競合検査より先に除く" '(lead "A l%12o4c")
                 (parameterize ([current-error-port (open-output-string)])
                   (let1 result (generate)
                     (list (car (slot-ref result 'track-names))
                           (find #/^A / (file->string-list (work-file "sources.mml") :encoding "cp932"))))))
          (define-track lead :part 'A :sound-source 'ssg)
          (test* "対応音源と固定パートの矛盾はエラー"
                 (test-error <sxmml-value-error> #/expected opn for part A/)
                 (parameterize ([current-error-port (open-output-string)]) (generate))))
        (test* "公開pmd-source出力の未定義音色は参照エラーになる"
               (test-error <sxmml-reference-error> #/undefined voice/)
               (pmd-ir-text "typed-reference" 'A
                            (range-ir (make <sxmml-mml-voice> :value 'UNKNOWN) 'A)))
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
                 (generated-text "reproducible" forms))
          (test* "未使用の定義を追加しても生成物は一致する"
                 (generated-text "unused" forms)
                 (generated-text "unused" (cons (voice-form 0 7) forms))))
        ;; 採番は出力されるFM音色指定の初出順。定義内容は AL で識別する。
        (let ([voice-lines (^[text] (filter-map (^l (and-let1 m (#/^@(\d{3}) (\d{3}) / l)
                                                     (list (string->number (m 1))
                                                           (string->number (m 2)))))
                                               (string-split text "\r\n")))]
              [a-line (^[text] (find #/^A / (string-split text "\r\n")))]
              [many-voices (^[n] (map (^i (voice-form (string->symbol (format "v~3,'0d" i)) 0))
                                      (iota n)))])
          (let1 text (generated-text "voice-first-use"
                                     `(,(voice-form 2 3) ,(voice-form 10 2) ,(voice-form 0 1)
                                       (mml A "@2c8")))
            (test* "数値の音色名にも初出順で番号を割り当てる"
                   '("A l%12@0o4c" ((0 3)))
                   (list (a-line text) (voice-lines text))))
          (let1 forms (append '((define-pcm-voice 0 12)
                                (let1 op (make-list 11 0) (define-opm-voice 0 '(0 0) op op op op)))
                              (map voice-form '(a b c) '(0 1 2))
                              '((mml A "@{b}@{c}c8")))
            (let1 text (pmd-text "used-voices-O0" forms 'pmd-source #f)
              (test* "最適化なしでは生成する全音色指定の定義だけを出力する"
                     '("A l%12@0@1o4c" ((0 1) (1 2)))
                     (list (a-line text) (voice-lines text))))
            (let1 text (pmd-text "used-voices-O1" forms 'pmd-source #t)
              (test* "最適化後の最初の音色を0番にする"
                     '("A l%12@0o4c" ((0 2)))
                     (list (a-line text) (voice-lines text)))))
          (let1 text (pmd-text "used-voices-ssg-pcm"
                              `(,(voice-form 0 0) (mml G "@0c8") (mml J "@0c8"))
                              'pmd-source #t)
            (test* "SSGとPCMの音色指定ではFM音色定義を出力しない"
                   '(() ("G l%12@0o4c" "J l%12@0o4c"))
                   (list (voice-lines text)
                         (filter #/^[GJ] / (string-split text "\r\n")))))
          (let1 text (pmd-text "used-voices-tracks"
                              (append (map voice-form '(lead unused bass pad) '(1 3 0 2))
                                      '((mml B "@{bass}c8@{lead}c8@{pad}c8")
                                        (mml A "@{pad}c8@{bass}c8")
                                        (mml G "c8") (mml J "c8")
                                        (rhythm L "@b8")))
                              'pmd-source #t)
            (test* "複数トラックの使用音色を集約し定義と全本文を順に出力する"
                   '("@000 002 000" "@001 000 000" "@002 001 000"
                     "A l%12@0o4c@1c" "B l%12@1o4c@2c@0c"
                     "G l%12o4c" "J l%12o4c"
                     "R0 \\br%12" "K l%12 R0")
                   (filter #/^(@\d|[A-JK] |R[01] )/
                           (string-split text "\r\n"))))
          (let1 text (pmd-text "used-voices-pmd3"
                              `(,(voice-form 'a 0) ,(voice-form 'b 1)
                                (mml A "@{b}c8"))
                              'pmd3-source #t)
            (test* "PMD3でも未使用定義を省きヘッダと終端を維持する"
                   '(((0 1)) #t "A l8@0o4c" 26)
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
          (let* ([forms (many-voices 257)]
                 [music (^[n] (string-concatenate
                               (map (^[i] (format "@{v~3,'0d}c8" i)) (iota n))))])
            (test* "257定義から256音色を使い、最後に255番を割り当てる"
                   (iota 256)
                   (map car (voice-lines (pmd-text "voices-256"
                                          (append forms `((mml A ,(music 256))))
                                          'pmd-source #f))))
            (test* "257種類目の参照で上限と件数を基底sxmml-errorで知らせる"
                   '(#t "too many voices: 257 (max 256)")
                   (guard (e [else (list (eq? (class-of e) <sxmml-error>) (condition-message e))])
                     (pmd-text "voices-257" (append forms `((mml A ,(music 257))))
                               'pmd-source #f)))))
        ;; LFO・エンベロープは、使うときに種類・値の数・値を検査し、何が悪いかを示して
        ;; 失敗する。種類は LFO が default・triangle・evolution、エンベロープが default (#28, #217)
        (let ([lfo (^[def channel] `(,def (mml G ,#"M{L1 ~channel}c8")))]
              [envelope (^[def] `(,def (mml G "E{E1}c8")))])
          (dolist [case `(("lfo-type" ,(lfo '(define-lfo L1 sw (list 1 2 3 4)) "A")
                           "unsupported LFO type: sw in L1 (supported: default, triangle, evolution)")
                          ("envelope-type" ,(envelope '(define-envelope E1 sw (list 1 2 3 4)))
                           "unsupported envelope type: sw in E1 (supported: default)")
                          ("unused-type" ((define-lfo L2 sw (list 1 2 3 4)) (mml G "c8"))
                           "G l%12o4c")
                          ("lfo-count" ,(lfo '(define-lfo L1 (list 1 2 3)) "A")
                           "invalid LFO values: L1 has 3 values (expected 4)")
                          ("lfo-count-1" ,(lfo '(define-lfo L1 (list 1)) "A")
                           "invalid LFO values: L1 has 1 value (expected 4)")
                          ("envelope-count-3" ,(envelope '(define-envelope E1 (list 1 2 3)))
                           "invalid envelope values: E1 has 3 values (expected 4, 5 or 6)")
                          ("envelope-count-7" ,(envelope '(define-envelope E1 (list 1 2 3 4 5 6 7)))
                           "invalid envelope values: E1 has 7 values (expected 4, 5 or 6)")
                          ("envelope-5" ,(envelope '(define-envelope E1 (list 1 2 3 4 5)))
                           "G l%12E1,2,3,4,5o4c")
                          ("lfo-inexact" ,(lfo '(define-lfo L1 (list 1 1.5 3 4)) "A")
                           "invalid LFO L1 value 2: 1.5 (expected an integer from 0 to 255)")
                          ("lfo-string" ,(lfo '(define-lfo L1 (list 1 2 "x" 4)) "A")
                           "invalid LFO L1 value 3: \"x\" (expected an integer from -128 to 127)")
                          ("lfo-list" ,(lfo '(define-lfo L1 (list 1 2 3 (list 1 2))) "A")
                           "invalid LFO L1 value 4: (1 2) (expected an integer from 0 to 255)")
                          ("lfo-range" ,(lfo '(define-lfo L1 (list 1 256 3 4)) "A")
                           "invalid LFO L1 value 2: 256 (expected an integer from 0 to 255)")
                          ("envelope-range-1" ,(envelope '(define-envelope E1 (list 1 16 3 4)))
                           "invalid envelope E1 value 2: 16 (expected an integer from -15 to 15)")
                          ("envelope-range-2" ,(envelope '(define-envelope E1 (list 32 2 3 4 5)))
                           "invalid envelope E1 value 1: 32 (expected an integer from 0 to 31)")
                          ("lfo-bounds" ,(lfo '(define-lfo L1 (list 0 255 -128 127)) "A")
                           "G l%12M0,255,-128,127o4c")
                          ;; 各値の範囲の両端は通る
                          ("lfo-min" ,(lfo '(define-lfo L1 (list 0 0 -128 0)) "A")
                           "G l%12M0,0,-128,0o4c")
                          ("lfo-max" ,(lfo '(define-lfo L1 (list 255 255 127 255)) "A")
                           "G l%12M255,255,127,255o4c")
                          ("envelope-4-min" ,(envelope '(define-envelope E1 (list 0 -15 0 0)))
                           "G l%12E0,-15,0,0o4c")
                          ("envelope-4-max" ,(envelope '(define-envelope E1 (list 255 15 255 255)))
                           "G l%12E255,15,255,255o4c")
                          ("envelope-5-min" ,(envelope '(define-envelope E1 (list 0 0 0 0 0)))
                           "G l%12E0,0,0,0,0o4c")
                          ("envelope-5-max" ,(envelope '(define-envelope E1 (list 31 31 31 15 15)))
                           "G l%12E31,31,31,15,15o4c")
                          ("envelope-6-min" ,(envelope '(define-envelope E1 (list 0 0 0 0 0 0)))
                           "G l%12E0,0,0,0,0,0o4c")
                          ("envelope-6-max" ,(envelope '(define-envelope E1 (list 31 31 31 15 15 15)))
                           "G l%12E31,31,31,15,15,15o4c")
                          ("lfo-channel" ,(lfo '(define-lfo L1 (list 1 2 3 4)) "C")
                           "invalid LFO channel: C")
                          ;; LFO の切り替えのチャンネルの誤りも、定義と同じメッセージにする
                          ("lfo-switch-channel" ((mml G "Mt{C}c8"))
                           "invalid LFO channel: C"))]
            (test* #"PMD checks LFO and envelope definitions: ~(car case)"
                   (caddr case)
                   (pmd-g-result (car case) (cadr case))))
          (test* "a triangle LFO is output the same as the default"
                 (pmd-g-result "lfo-default" (lfo '(define-lfo L1 (list 1 2 3 4)) "A"))
                 (pmd-g-result "lfo-triangle" (lfo '(define-lfo L1 triangle (list 1 2 3 4)) "A")))
          ;; evolution は PMD の MD/MDB として出力する (#217)
          (dolist [case `(("evolution-1-a" ((define-lfo EV1 evolution (list 3)) (mml G "M{EV1 A}c8"))
                           "G l%12MD3,0o4c")
                          ("evolution-1-b" ((define-lfo EV1 evolution (list 3)) (mml G "M{EV1 B}c8"))
                           "G l%12MDB3,0o4c")
                          ("evolution-1-default-ch" ((define-lfo 1 evolution (list 0)) (mml G "M1c8"))
                           "G l%12MD0,0o4c")
                          ("evolution-2-a" ((define-lfo EV1 evolution (list 3 6)) (mml G "M{EV1 A}c8"))
                           "G l%12MD3,6o4c")
                          ("evolution-2-b-neg" ((define-lfo EV1 evolution (list 3 -6)) (mml G "M{EV1 B}c8"))
                           "G l%12MDB3,-6o4c")
                          ("evolution-3-a" ((define-lfo EV1 evolution (list 1 2 4)) (mml G "M{EV1 A}c8"))
                           "G l%12MD1,2,4o4c")
                          ("evolution-3-b" ((define-lfo EV1 evolution (list 1 2 4)) (mml G "M{EV1 B}c8"))
                           "G l%12MDB1,2,4o4c")
                          ("evolution-3-zero-time" ((define-lfo EV1 evolution (list 1 2 0)) (mml G "M{EV1 A}c8"))
                           "G l%12MD1,2,0o4c")
                          ("evolution-min" ((define-lfo EV1 evolution (list 0 -128 0)) (mml G "M{EV1 A}c8"))
                           "G l%12MD0,-128,0o4c")
                          ("evolution-max" ((define-lfo EV1 evolution (list 255 127 127)) (mml G "M{EV1 A}c8"))
                           "G l%12MD255,127,127o4c")
                          ("evolution-zero-speed-3" ((define-lfo EV1 evolution (list 0 0 1)) (mml G "M{EV1 A}c8"))
                           "G l%12MD0,0,1o4c")
                          ("evolution-unused" ((define-lfo EV1 evolution (list 1 2 3 4)) (mml G "c8"))
                           "G l%12o4c")
                          ("evolution-count-0" ((define-lfo EV1 evolution '()) (mml G "M{EV1 A}c8"))
                           "invalid LFO values: EV1 has 0 values (expected 1, 2 or 3)")
                          ("evolution-count-4" ((define-lfo EV1 evolution (list 1 2 3 4)) (mml G "M{EV1 A}c8"))
                           "invalid LFO values: EV1 has 4 values (expected 1, 2 or 3)")
                          ("evolution-inexact" ((define-lfo EV1 evolution (list 1.5 2)) (mml G "M{EV1 A}c8"))
                           "invalid LFO EV1 value 1: 1.5 (expected an integer from 0 to 255)")
                          ("evolution-v1-range" ((define-lfo EV1 evolution (list 256 0)) (mml G "M{EV1 A}c8"))
                           "invalid LFO EV1 value 1: 256 (expected an integer from 0 to 255)")
                          ("evolution-v2-low" ((define-lfo EV1 evolution (list 1 -129)) (mml G "M{EV1 A}c8"))
                           "invalid LFO EV1 value 2: -129 (expected an integer from -128 to 127)")
                          ("evolution-v2-high" ((define-lfo EV1 evolution (list 1 128)) (mml G "M{EV1 A}c8"))
                           "invalid LFO EV1 value 2: 128 (expected an integer from -128 to 127)")
                          ("evolution-v3-low" ((define-lfo EV1 evolution (list 1 2 -1)) (mml G "M{EV1 A}c8"))
                           "invalid LFO EV1 value 3: -1 (expected an integer from 0 to 127)")
                          ("evolution-v3-high" ((define-lfo EV1 evolution (list 1 2 128)) (mml G "M{EV1 A}c8"))
                           "invalid LFO EV1 value 3: 128 (expected an integer from 0 to 127)")
                          ("evolution-channel" ((define-lfo EV1 evolution (list 1 2)) (mml G "M{EV1 C}c8"))
                           "invalid LFO channel: C")
                          ("evolution-undefined" ((mml G "M{EV9 A}c8"))
                           "undefined LFO: EV9"))]
            (test* #"PMD checks evolution LFO definitions: ~(car case)"
                   (caddr case)
                   (pmd-g-result (car case) (cadr case))))
          (let ([g-line (^[text] (find #/^G / (string-split text "\r\n")))])
            (test* "pmd3-source outputs 3-value and B-channel evolution as-is"
                   "G l8MD1,2,4MDB3,6MD0,0,1o4c"
                   (g-line (pmd-text "pmd3-evolution"
                                     '((define-lfo EV3 evolution (list 1 2 4))
                                       (define-lfo EV2 evolution (list 3 6))
                                       (define-lfo EV0 evolution (list 0 0 1))
                                       (mml G "M{EV3 A}M{EV2 B}M{EV0 A}c8"))
                                     'pmd3-source #t)))
            (dolist [case `((#t "G l%12M1,2,3,4*1MD1,2,4MD3,6o4cMD3,6M1,2,3,4*0d")
                            (:full "G l%12M1,2,3,4*1MD1,2,4MD3,6o4cMD3,6*0d"))]
              (test* #"optimization preserves M and evolution LFO (~(car case))"
                     (cadr case)
                     (g-line (pmd-text #"opt-evolution-~(car case)"
                                       '((define-lfo L1 (list 1 2 3 4))
                                         (define-lfo EV3 evolution (list 1 2 4))
                                         (define-lfo EV2 evolution (list 3 6))
                                         (mml G "M{L1 A}MtM{EV3 A}M{EV2 A}c8M{EV2 A}M{L1 A}Mfd8"))
                                       'pmd-source (car case)))))
            ;; L1 の再指定は状態追跡（:full）で省かれるが、evolution の再指定は残る
            (test* "state optimization keeps repeated evolution while dropping redundant M"
                   "G l%12M1,2,3,4MD3,6o4cMD3,6d"
                   (g-line (pmd-text "opt-evolution-state"
                                     '((define-lfo L1 (list 1 2 3 4))
                                       (define-lfo EV2 evolution (list 3 6))
                                       (mml G "M{L1 A}M{EV2 A}c8M{EV2 A}M{L1 A}d8"))
                                     'pmd-source :full)))
            (test* "repeat compression folds repeated evolution commands"
                   "G l%12[MD3,6o4c]4"
                   (g-line (pmd-text "opt-evolution-repeat"
                                     '((define-lfo EV2 evolution (list 3 6))
                                       (mml G "o4M{EV2 A}c8M{EV2 A}c8M{EV2 A}c8M{EV2 A}c8"))
                                     'pmd-source :repeat))))
          (let* ([env (make <sxmml-output-environment>)]
                 [_ (hash-table-put! (slot-ref env 'lfo) 'L1 (make <sxmml-lfo> :id 'L1 :type 'default :value '(1 2 3 4)))]
                 [_ (hash-table-put! (slot-ref env 'lfo) 'EV1 (make <sxmml-lfo> :id 'EV1 :type 'evolution :value '(3 6)))]
                 [ir (list (make <sxmml-mml-lfo> :value 'L1 :channel 'A)
                           (make <sxmml-mml-lfo> :value 'EV1 :channel 'B)
                           (make <sxmml-mml-lfo> :value 'MISSING :channel 'A)
                           (make <sxmml-mml-lfo-switch> :value #t :channel 'A))]
                 [prepared (sxmml-pmd-ir-prepare ir env)])
            (test* "sxmml-pmd-ir-prepare converts only defined evolution LFOs"
                   (list <sxmml-mml-lfo> <sxmml-pmd-lfo-evolution> <sxmml-mml-lfo> <sxmml-mml-lfo-switch>
                         'EV1 'B #f)
                   (list (class-of (list-ref prepared 0))
                         (class-of (list-ref prepared 1))
                         (class-of (list-ref prepared 2))
                         (class-of (list-ref prepared 3))
                         (slot-ref (list-ref prepared 1) 'value)
                         (slot-ref (list-ref prepared 1) 'channel)
                         (is-a? (list-ref prepared 1) <sxmml-mml-lfo>)))))
        ;; PMD が使うメタデータは、title などが文字列、memo が文字列か文字列のリストで、
        ;; それ以外は書くときにキーと値を示して失敗する (#13)
        (dolist [case `(("title-number" pmd-source ((define-metadata title 1))
                         "invalid metadata value: title is 1 (expected a string)")
                        ("title-list" pmd-source ((define-metadata title (list "t1" "t2")))
                         "invalid metadata value: title is (\"t1\" \"t2\") (expected a string)")
                        ("memo-element" pmd-source ((define-metadata memo (list "a" 1)))
                         "invalid metadata value: memo is (\"a\" 1) (expected a string or a list of strings)")
                        ("memo-number" pmd-source ((define-metadata memo 1))
                         "invalid metadata value: memo is 1 (expected a string or a list of strings)")
                        ("memo-empty" pmd-source ((define-metadata memo '())) ())
                        ("strings" pmd-source ((define-metadata title "t") (define-metadata composer "c"))
                         ("#title       t" "#composer    c"))
                        ("pmd3-title-number" pmd3-source ((define-metadata title 1))
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
                                                          '((define-envelope E4 (list 1 2 3 4))
                                                            (define-envelope E5 (list 1 2 3 4 5))
                                                            (define-envelope E6 (list 1 2 3 4 5 6))
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
                           (^[] (sxmml-voice-set! 'opn
                                                (make <sxmml-opn-voice> :id 'tone
                                                      :value (cons alfb (make-list 4 op)))))))
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
          (test* "手作り音色の負のDTは出力時に正規化せず拒否する"
                 (range-message "FM voice DT" -1 0 7)
                 (voice-result "raw-negative-dt" '(0 0) '(0 0 0 0 0 0 0 0 -1 0 0 0)))
          (test* "手作り音色の非出力項目DT2とSEGを追加検査しない"
                 '("@000 000 000" "A @0o4c%12")
                 (voice-result "raw-ignored-fields" '(0 0) '(0 0 0 0 0 0 0 0 0 4 16 0)))
          (let1 registered-lines
              (^[dt] (filter #/^(@\d| \d)/
                             (string-split
                              (pmd-ir-text #"registered-dt-~dt" 'A ir
                                           (^[] (let1 op (list 0 0 0 0 0 0 0 0 dt)
                                                  (sxmml-opn-voice-set! 'tone '(0 0) op op op op))))
                              "\r\n")))
            (test* "負数表記と非負表記のDTは同じ音色行を出力する"
                   #t (equal? (registered-lines -1) (registered-lines 5))))
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
                                       (if (equal? case '("J" "x")) #/undefined PCM voice: x$/
                                       (string->regexp
                                        #"invalid ~(if (equal? (car case) \"J\") \"PCM\" \"SSG\") voice number: ~(regexp-quote (cadr case)) \\(expected an integer from 0 to ~(if (equal? (car case) \"J\") 255 9)\\)$"))))))
      )
    (remove-directory* workdir)))

(parameterize ([sxmml-environment (make <sxmml-environment>)])
  (define (resolve value)
    (sxmml-pcm-voice-set! 'kick value)
    (sxmml-pmd-pcm-voice-number (sxmml-environment) 'kick))
  (test* "PCM番号と先頭ファイル位置はファイル指定を必要としない"
         '(25 26) (list (resolve 25) (resolve '(0 26))))
  (define-metadata pcmfile "drums.ppc")
  (test* "登録ファイル名を完全一致で解決する" 25 (resolve '("drums.ppc" 25)))
  (test* "ファイル名の大小文字を区別する" (test-error <sxmml-value-error>) (resolve '("Drums.ppc" 25)))
  (test* "Jのファイル位置は0だけ" (test-error <sxmml-value-error>) (resolve '(1 25)))
  (define-pcm-voice 0 25)
  (test* "数値名の定義を直指定より優先する" '(25 1)
         (map (cut sxmml-pmd-pcm-voice-number (sxmml-environment) <>) '(|0| |1|)))
  (let1 metadata (slot-ref (sxmml-environment) 'metadata)
    (define (setting key value)
      (hash-table-put! metadata key value)
      (sxmml-pmd-metadata-value metadata key))
    (test* "PPZ文字列とファイルリスト" '("a" "a,b" "a, b")
           (list (setting 'pcmfile '("a")) (setting 'ppzfile '("a" "b")) (setting 'ppzfile "a, b")))
    (test* "PCMファイルは最大1個" (test-error <sxmml-value-error>) (setting 'pcmfile '("a" "b")))
    (test* "PPZファイルは最大2個" (test-error <sxmml-value-error>) (setting 'ppzfile '("a" "b" "c")))))

(parameterize ([sxmml-environment (make <sxmml-environment>)])
  (define (first-track)
    (car (sxmml-pmd-output-tracks-select
          (make-sxmml-output-environment (sxmml-environment) :output #f :optimize 0))))
  (define-track A)
  (define-track lead :part 'A)
  (test* "PMD出力時に名前の補完と明示partが衝突した両名を示す"
         (test-error <sxmml-value-error> "conflicting PMD part A: track A (implicit) and track lead (explicit)")
         (first-track))
  (sxmml-track-delete! 'lead)
  (test* "PMDは未指定パートに改名時点の名前を使う" '(A #f)
         (let1 name (slot-ref (first-track) 'name)
           (rename-track A lead)
           (list name (first-track)))))

(parameterize ([sxmml-environment (make <sxmml-environment>)])
  (define (number id) (sxmml-pmd-pcm-voice-number (sxmml-environment) id 'ppz8))
  (define-pcm-voice kick '(1 0))
  (define-pcm-voice named '("drums" 0))
  (define-pcm-voice invalid 128)
  (test* "PPZ8の直接指定と第2バンクの先頭" '(128 128) (list (number '|128|) (number 'kick)))
  (dolist [files '("base,drums" ("base" "drums"))]
    (define-metadata ppzfile files)
    (test* "PPZ8のファイル名を指定順に解決する" 128 (number 'named)))
  (test* "ファイル内の128番は範囲外" (test-error <sxmml-value-error>) (number 'invalid)))

(test-end :exit-on-failure #t)
