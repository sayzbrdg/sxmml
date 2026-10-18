;;;
;;; Test sxmml
;;;

(use gauche.test)

(test-start "sxmml")
(use sxmml.classes)
(test-module 'sxmml.classes)
(use sxmml.mml)
(test-module 'sxmml.mml)
(use sxmml.environment)
(test-module 'sxmml.environment)
(use sxmml.voice)
(test-module 'sxmml.voice)
(use sxmml.compiler)
(test-module 'sxmml.compiler)
(use sxmml.processor)
(test-module 'sxmml.processor)
(use sxmml.optimizer)
(test-module 'sxmml.optimizer)
(use sxmml.util)
(test-module 'sxmml.util)
(use sxmml.form)
(test-module 'sxmml.form)
(use sxmml)
(use sxmml.backend.pmd)
(test-module 'sxmml)

;; 定義の誤りの位置 (#28)
;; sxmml-form-call は、中で起きたエラーに定義の位置を付けた <sxmml-form-error> を投げる
(let ([caught (^[thunk] (guard (e [else e]) (thunk)))]
      [boom (^[] (errorf "boom: ~a" 1))])
  (test* "sxmml-form-call adds the location to an error"
         '(#t "./a.sxl:3: boom: 1" "./a.sxl:3" "boom: 1")
         (let1 e (caught (^[] (sxmml-form-call "./a.sxl:3" boom)))
           (list (sxmml-form-error? e) (condition-message e) (sxmml-form-error-location e)
                 (condition-message (sxmml-form-error-cause e)))))
  (test* "sxmml-form-call without a location passes the error"
         '(#f "boom: 1")
         (let1 e (caught (^[] (sxmml-form-call #f boom)))
           (list (sxmml-form-error? e) (condition-message e))))
  ;; 入れ子では、いちばん内側の定義の位置だけを残す
  (test* "sxmml-form-call passes an inner form error"
         '("./b.sxl:7: boom: 1" "./b.sxl:7")
         (let1 e (caught (^[] (sxmml-form-call "./a.sxl:3"
                                (^[] (sxmml-form-call "./b.sxl:7" boom)))))
           (list (condition-message e) (sxmml-form-error-location e))))
  (test* "sxmml-form-call passes a raised non-condition"
         42
         (caught (^[] (sxmml-form-call "./a.sxl:3" (^[] (raise 42))))))
  ;; 位置を付けるのはエラーだけで、続けられるエラーでない condition は、そのまま通して続けさせる
  (test* "sxmml-form-call passes a continuable non-error condition"
         '(handled 10)
         (with-exception-handler
          (^[c] (if (and (message-condition? c) (not (error? c))) 'handled (raise c)))
          (^[] (sxmml-form-call "./a.sxl:3"
                 (^[] (list (raise-continuable (make-condition <message-condition> 'message "note"))
                            10))))))
  (test* "sxmml-form-call returns the value of the thunk"
         5
         (sxmml-form-call "./a.sxl:3" (^[] 5)))
  (test* "sxmml-form-location of a constructed form is #f"
         #f
         (sxmml-form-location (list 'a 'b)))
  ;; 定義のマクロは、登録の呼び出しを sxmml-form-wrap で包んで展開する
  (test* "sxmml-form-wrap wraps a call with sxmml-form-call"
         '(sxmml-form-call #f (lambda () (foo 1)))
         (sxmml-form-wrap (list 'define-x 'a) (^[x] x) '(foo 1)))
  (test* "sxmml-form-error without a location"
         '(#t "bad: 1" #f #f)
         (let1 e (caught (^[] (sxmml-form-error (list 'a) "bad: ~a" 1)))
           (list (sxmml-form-error? e) (condition-message e)
                 (sxmml-form-error-location e) (sxmml-form-error-cause e)))))

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
  (dolist [case `(("a non-symbol voice id" "voice id must be a symbol: 0" (0 (0 0) ,@ops))
                  ("an invalid al/fb" "invalid voice format: @v, al/fb, (0)" (v (0) ,@ops))
                  ("an invalid operator" "invalid voice format: @v, operator(2), (0 0 0)"
                   (v (0 0) ,op0 (0 0 0) ,op0 ,op0))
                  ("a negative value" "invalid voice value: @v, operator(2), -1"
                   (v (0 0) ,op0 (-1 0 0 0 0 0 0 0 0) ,op0 ,op0))
                  ("an inexact value" "invalid voice value: @v, operator(2), 1.5"
                   (v (0 0) ,op0 (1.5 0 0 0 0 0 0 0 0) ,op0 ,op0))
                  ("a non-number value" "invalid voice value: @v, operator(2), x"
                   (v (0 0) ,op0 (x 0 0 0 0 0 0 0 0) ,op0 ,op0))
                  ("a negative al/fb" "invalid voice value: @v, al/fb, -1" (v (0 -1) ,@ops)))]
    (test* #"~(car case) fails" (test-error <error> (cadr case))
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
  (test* "infinite repeat expands once" #t
         (let ([s (make <sxmml-mml-repeat-start>)] [b (make <sxmml-mml-repeat-break>)]
               [z (make <sxmml-mml-repeat-end> :value 0)])
           (same-objects? (list c d) (expansion (structure (list s c b d z))))))
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

;; 反復の : は一つの反復に一つだけ (#92)
(let1 compile (^[text] (parameterize ([sxmml-environment (make <sxmml-environment>)])
                         (define-track G)
                         (mml G text)
                         'compiled))
  (test* "a second break in a repeat fails"
         (test-error <error> "break command can be used only once in a repeat")
         (compile "[c:d:e]2"))
  (test* "breaks in nested repeats are accepted" 'compiled
         (compile "[c:[d:e]2f]2"))
  (test* "a break outside of repeats fails"
         (test-error <error> "break command cannot be used outside of repeats")
         (compile ":c")))

;; sxlc の CLI
(use gauche.process)
(use file.util)
(let* ([srcdir (sys-normalize-pathname (sys-dirname (current-load-path)) :absolute #t)]
       [workdir (sys-mkdtemp (build-path (temporary-directory) "sxmml-test"))])
  ;; name.sxl に forms を書き出し、options を付けて sxlc を実行して出力行を返す
  (define (run-sxlc name forms . options)
    (let1 source (build-path workdir #"~|name|.sxl")
      (with-output-to-file source (^[] (for-each write forms)))
      (process-output->string-list
       `("gosh" ,#"-I~srcdir" ,(build-path srcdir "sxlc.in") ,@options ,source)
       :error :merge)))
  (define (generated-mode-line name)
    (find #/^;; mode:/ (file->string-list (build-path workdir #"~|name|.mml")
                                          :encoding "shift_jis")))
  ;; sxlc の出力行から、Translate に表示した出力先、-c の後処理に渡った名前、
  ;; filename メタデータの警告の有無を取り出す
  (define (output-report lines)
    (list (and-let1 m (any #/^Translate: .* => (.*)$/ lines) (m 1))
          (and-let1 m (any #/^<(.*)>$/ lines) (m 1))
          (boolean (find #/^warning: metadata 'filename'/ lines))))
  ;; name.sxl を PMD モードで変換し、-c で生成名を表示させて output-report を返す
  (define (run-output-check name forms . options)
    (output-report
     (apply run-sxlc name forms "-m" "pmd" "-c" "printf '<%s>\\n'" options)))
  (define (work-file name) (build-path workdir name))
  ;; 作業ディレクトリ name に既存の生成物 out.mml と空のディレクトリ adir を置き、
  ;; text を書いた入力を -o output で変換して、終了値が 0 以外か、
  ;; ディレクトリの内容（名前と中身）が変換前と同じかを返す
  (define (run-failure-check name text output)
    (let* ([dir (work-file name)]
           [source (build-path dir "in.sxl")]
           [snapshot (^[] (map (^f (let1 path (build-path dir f)
                                     (cons f (if (file-is-directory? path)
                                               (directory-list path :children? #t)
                                               (file->string path)))))
                               (directory-list dir :children? #t)))])
      (make-directory* (build-path dir "adir"))
      (with-output-to-file source (^[] (display text)))
      (with-output-to-file (build-path dir "out.mml") (^[] (display "existing output\n")))
      (let* ([before (snapshot)]
             [p (run-process `("gosh" ,#"-I~srcdir" ,(build-path srcdir "sxlc.in")
                               "-m" "pmd" "-o" ,output ,source)
                             :directory dir :output :null :error :null :wait #t)])
        (list (not (zero? (process-exit-status p)))
              (equal? before (snapshot))))))
  ;; 音色 id を定義する式。音色の値で出力を区別できるよう AL に al を使う
  (define (voice-form id al)
    `(define-opn-voice ,id (,al 0) ,@(make-list 4 '(0 0 0 0 0 0 0 0 0))))
  ;; name.sxl の変換結果の生成物の内容を返す
  (define (generated-text name forms)
    (run-sxlc name forms "-m" "pmd")
    (file->string (work-file #"~|name|.mml")))
  ;; name.sxl に text を書いて返す
  (define (write-source name text)
    (rlet1 source (work-file #"~|name|.sxl")
      (with-output-to-file source (^[] (display text)))))
  ;; args を付けて sxlc を実行し、終了値と標準エラー出力の行のリストを返す
  (define (sxlc-status . args)
    (let* ([err (work-file "status.err")]
           [p (run-process `("gosh" ,#"-I~srcdir" ,(build-path srcdir "sxlc.in") ,@args)
                           :output :null :error err :wait #t)])
      (list (sys-wait-exit-status (process-exit-status p)) (file->string-list err))))
  ;; prefix のコマンドを前に付けて args で sxlc を実行し、終了値と、標準出力と
  ;; 標準エラー出力の行のリストを返す。出力は復号できないバイト列もそのまま読む
  (define (sxlc-run prefix . args)
    (let* ([status 0]
           [lines (process-output->string-list
                   `(,@prefix "gosh" ,#"-I~srcdir" ,(build-path srcdir "sxlc.in") ,@args)
                   :error :merge
                   :on-abnormal-exit (^p (set! status (process-exit-status p))))])
      (list (sys-wait-exit-status status) lines)))
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
  (unwind-protect
      (begin
        ;; -c の後処理へ渡す生成名
        (dolist [name '("semi;name" "space name")]
          (test* #"-c receives \"~|name|.mml\" as one argument"
                 #"<~(build-path workdir name).mml>"
                 (find #/^</ (run-sxlc name '((mml G "c8"))
                                       "-m" "pmd" "-c" "printf '<%s>\\n'"))))
        ;; CLI とソースのモード指定の優先順位
        (test* "sxmml-mode in source selects the mode without -m"
               '("Processing Mode: PMD3" ";; mode: PMDver3.3")
               (list (find #/^Processing Mode:/
                           (run-sxlc "source-mode" '((sxmml-mode pmd3) (mml G "c8"))))
                     (generated-mode-line "source-mode")))
        (test* "-m takes precedence over sxmml-mode in source"
               '("warning: sxmml-mode in source is ignored; mode is already set to PMD"
                 "Processing Mode: PMD" ";; mode: PMDver4.8")
               (let1 lines (run-sxlc "cli-mode" '((sxmml-mode pmd3) (mml G "c8")) "-m" "pmd")
                 (list (find #/^warning:/ lines)
                       (find #/^Processing Mode:/ lines)
                       (generated-mode-line "cli-mode"))))
        ;; 出力先の決定 (#4)
        ;; 出力先は sxlc の -o、なければ入力名の拡張子を .mml に替えたものとし、
        ;; Translate の表示、生成先、-c に渡す名前が一致すること
        (let1 out (work-file "custom-out.mml")
          (test* "-o sets the output file"
                 (list out out #f #t #f)
                 (append (run-output-check "opt-o" '((mml G "c8")) "-o" out)
                         (list (file-exists? out)
                               (file-exists? (work-file "opt-o.mml"))))))
        (test* "-o with a relative path is resolved against the current directory"
               (list (work-file "rel-custom.mml") #t #f)
               (let1 source (work-file "rel-out.sxl")
                 (with-output-to-file source (^[] (write '(mml G "c8"))))
                 (let1 lines (process-output->string-list
                              `("gosh" ,#"-I~srcdir" ,(build-path srcdir "sxlc.in")
                                "-m" "pmd" "-o" "rel-custom.mml" ,source)
                              :directory workdir :error :merge)
                   (list (car (output-report lines))
                         (file-exists? (work-file "rel-custom.mml"))
                         (file-exists? (work-file "rel-out.mml"))))))
        (let ([meta (work-file "meta.mml")]
              [default (work-file "filename-meta.mml")])
          (test* "filename metadata is ignored with a warning"
                 (list default default #t #t #f)
                 (append (run-output-check "filename-meta"
                                           `((define-metadata filename ,meta) (mml G "c8")))
                         (list (file-exists? default) (file-exists? meta)))))
        (let1 default (work-file "default-out.mml")
          (test* "the output file defaults to the source name with .mml"
                 (list default default #f #t)
                 (append (run-output-check "default-out" '((mml G "c8")))
                         (list (file-exists? default)))))
        (test* "sxmml-output-file is not exported"
               #f
               (boolean (memq 'sxmml-output-file
                              (module-exports (find-module 'sxmml.environment)))))
        (let ([slot-out (work-file "slot.mml")]
              [default (work-file "slot-set.mml")])
          (test* "the output slot set in source is overridden by sxlc"
                 (list default default #f #t #f)
                 (append (run-output-check "slot-set"
                                           `((slot-set! (sxmml-environment) 'output ,slot-out)
                                             (mml G "c8")))
                         (list (file-exists? default) (file-exists? slot-out)))))
        ;; 失敗時の生成物 (#16)
        ;; 読み込み・生成・出力のいずれで失敗しても、0 以外の終了値で終了し、
        ;; 既存の生成物を変更せず、途中までの生成物や一時ファイルを残さないこと
        (dolist [c '(("fail-read" "(mml G \"c8\"" "out.mml" "a read error")
                     ("fail-generate" "(mml A \"@0c8\")" "out.mml" "an undefined voice")
                     ("fail-no-dir" "(mml G \"c8\")" "nodir/out.mml" "a missing output directory")
                     ("fail-dir" "(mml G \"c8\")" "adir" "an output path that is a directory"))]
          (test* #"~(cadddr c) fails without changing existing files"
                 '(#t #t)
                 (run-failure-check (car c) (cadr c) (caddr c))))
        ;; 生成物の再現性 (#16)
        ;; 音色定義は番号順に出力し、同じ入力からはバイト単位で同じ生成物が得られること
        (test* "voice definitions are output in number order"
               '(0 1 2 3 4 5 6 7 8 9)
               (map (^m (string->number (m 1)))
                    (filter-map #/^@(\d+)/
                                (string-split (generated-text
                                               "voice-order"
                                               (append (map voice-form '(j i h g f e d c b a) (iota 10))
                                                       '((mml A "@{a}c8"))))
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
                   '("A l%12@2o4c" ((0 1) (1 2) (2 3)))
                   (list (a-line text) (voice-lines text))))
          (let1 text (generated-text "voice-in-begin"
                                     `((begin ,(voice-form 'b 1) ,(voice-form 'a 2))
                                       (mml A "@{a}c8@{b}c8")))
            (test* "voices defined in begin get distinct numbers"
                   '("A l%12@0o4c@1c" ((0 2) (1 1)))
                   (list (a-line text) (voice-lines text))))
          (dolist [case '((256 0 #t) (257 1 #f))]
            (let* ([n (car case)]
                   [output (work-file #"voices-~|n|.mml")]
                   [source (write-source #"voices-~n"
                                         (with-output-to-string
                                           (^[] (for-each write (append (many-voices n)
                                                                        '((mml A "@{v000}c8")))))))]
                   [result (sxlc-status "-m" "pmd" "-o" output source)])
              (test* #"~n voices ~(if (zero? (cadr case)) \"are accepted\" \"fail\")"
                     (list (cadr case) (not (caddr case)) (caddr case))
                     (list (car result)
                           (boolean (any #/too many voices: 257 \(max 256\)/ (cadr result)))
                           (file-exists? output))))))
        ;; define-opn-voice の枠の中の要素は式として評価する。定義の誤りは、展開時でも
        ;; 実行時でも、フォームの開始位置を付けた <sxmml-form-error> とする (#28)
        (let* ([op0 "(0 0 0 0 0 0 0 0 0)"]
               ;; 3 行目の begin の中の定義。op2 と al/fb を差し替えられる
               [voice-text (^[alfb op2] #"; voice\n(begin\n  (define-opn-voice V1 ~alfb\n    ~op0 ~op2 ~op0 ~op0)\n  (define %test-after-voice 1))\n")]
               [load-result (^[name text]
                              (let1 source (write-source name text)
                                (parameterize ([sxmml-environment (make <sxmml-environment>)]
                                               [sxmml-source-file source])
                                  (guard (e [else (cons source e)])
                                    (load source)
                                    (cons source (hash-table-get (slot-ref (sxmml-environment) 'voice) 'V1 #f))))))]
               [error-report (^[r] (let1 e (cdr r)
                                     (list (sxmml-form-error? e) (condition-message e)
                                           (sxmml-form-error-location e))))])
          (let1 r (load-result "voice-evaluated"
                               #"(define %test-al 3)\n(define-opn-voice V1 (%test-al 0) (1 (+ 1 2) 0 0 0 0 0 0 0) ~op0 ~op0 ~op0)\n")
            (test* "elements of define-opn-voice are evaluated"
                   '((3 0) (1 3 0 0 0 0 0 0 0 0 0 0))
                   (let1 v (slot-ref (cdr r) 'value) (list (car v) (cadr v)))))
          (let1 r (load-result "voice-wrong-operator" (voice-text "(0 0)" "(0 0 0)"))
            (test* "a wrong operator in begin fails with its location"
                   (list #t #"~(car r):3: invalid voice format: @V1, operator(2), (0 0 0)" #"~(car r):3" #f)
                   (append (error-report r) (list (sxmml-form-error-cause (cdr r))))))
          ;; 要素の数の誤りは展開時に検査するので、cause を持たない
          (dolist [case `(("voice-wrong-alfb" "(0)" ,op0 "al/fb, (0)")
                          ("voice-8-values" "(0 0)" "(0 0 0 0 0 0 0 0)" "operator(2), (0 0 0 0 0 0 0 0)")
                          ("voice-13-values" "(0 0)" "(0 0 0 0 0 0 0 0 0 0 0 0 0)"
                           "operator(2), (0 0 0 0 0 0 0 0 0 0 0 0 0)"))]
            (let1 r (load-result (car case) (voice-text (cadr case) (caddr case)))
              (test* #"a wrong number of values fails at expansion with its location: ~(car case)"
                     (list #t #"~(car r):3: invalid voice format: @V1, ~(cadddr case)" #"~(car r):3" #f)
                     (append (error-report r) (list (sxmml-form-error-cause (cdr r)))))))
          (dolist [case `(("voice-three-operators" ,#"; voice\n(begin\n  (define-opn-voice V1 (0 0)\n    ~op0 ~op0 ~op0)\n  1)\n")
                          ("voice-variable-operator" ,(voice-text "(0 0)" "op")))]
            (let1 r (load-result (car case) (cadr case))
              (test* #"a wrong define-opn-voice form fails with its location: ~(car case)"
                     (list #t #"~(car r):3: invalid define-opn-voice form: expected (define-opn-voice id (al fb) op1 op2 op3 op4)" #"~(car r):3")
                     (error-report r))))
          (let1 r (load-result "voice-wrong-value" (voice-text "(0 0)" "(-1 0 0 0 0 0 0 0 0)"))
            (test* "a wrong voice value fails with its location"
                   (list #t #"~(car r):3: invalid voice value: @V1, operator(2), -1" #"~(car r):3"
                         "invalid voice value: @V1, operator(2), -1")
                   (append (error-report r)
                           (list (condition-message (sxmml-form-error-cause (cdr r)))))))
          (let1 r (load-result "voice-undefined-element" (voice-text "(0 0)" "(%test-undefined-element 0 0 0 0 0 0 0 0)"))
            (test* "an undefined variable in an element fails with its location"
                   (list #t #t #"~(car r):3" #t)
                   (let1 e (cdr r)
                     (list (sxmml-form-error? e)
                           (boolean (#/^\S+:3: / (condition-message e)))
                           (sxmml-form-error-location e)
                           (is-a? (sxmml-form-error-cause e) <unbound-variable-error>)))))
          (let1 source (write-source "voice-sxlc-location"
                                     (string-append "(mml G \"c8\")\n" (voice-text "(0 0)" "(-1 0 0 0 0 0 0 0 0)")))
            (test* "sxlc shows the location of a wrong voice"
                   '(1 #t)
                   (status-and-message (sxlc-status "-m" "pmd" "-o" (work-file "voice-sxlc-location.mml") source)
                                       (string->regexp
                                        (regexp-quote #"*** SXMML-FORM-ERROR: ~|source|:4: invalid voice value"))))))
        ;; define-lfo・define-envelope の枠の中の値は式として評価し、種類はリテラルとする。
        ;; 形の誤りと値の式の誤りには、定義の開始位置を付ける (#28)
        (dolist [def '((define-lfo lfo) (define-envelope envelope))]
          (let* ([macro (car def)]
                 [table (cadr def)]
                 [load-result (^[name text]
                                (let1 source (write-source name text)
                                  (parameterize ([sxmml-environment (make <sxmml-environment>)]
                                                 [sxmml-source-file source])
                                    (guard (e [else (cons source e)])
                                      (load source)
                                      (cons source (hash-table-get (slot-ref (sxmml-environment) table) 'X1 #f))))))]
                 [registered (^[name text]
                               (let1 obj (cdr (load-result name text))
                                 (list (slot-ref obj 'type) (slot-ref obj 'value))))]
                 [form-message #"invalid ~|macro| form: expected (~|macro| id [type] (value ...))"])
            (test* #"elements of ~macro are evaluated"
                   '(default (1 2 3 4))
                   (registered #"~|macro|-evaluated"
                               #"(define %test-depth 3)\n(~macro X1 (1 2 %test-depth (+ 1 3)))\n"))
            (test* #"~macro takes a literal type"
                   '(sw (1 2 3 4))
                   (registered #"~|macro|-type" #"(define sw 1)\n(~macro X1 sw (1 2 3 4))\n"))
            (test* #"~macro accepts an empty frame"
                   '(default ())
                   (registered #"~|macro|-empty" #"(~macro X1 ())\n"))
            (dolist [case '(("no-frame" "") ("not-list" " 1") ("two-frames" " (1 2) (3 4)")
                            ("non-symbol-type" " 1 (1 2)") ("too-many" " sw (1 2) 3"))]
              (let1 r (load-result #"~|macro|-~(car case)" #"; def\n(begin\n  (~macro X1~(cadr case))\n  1)\n")
                (test* #"a wrong ~macro form fails with its location: ~(car case)"
                       (list #t #"~(car r):3: ~form-message" #"~(car r):3" #f)
                       (let1 e (cdr r)
                         (list (sxmml-form-error? e) (condition-message e)
                               (sxmml-form-error-location e) (sxmml-form-error-cause e))))))
            (let1 r (load-result #"~|macro|-undefined" #"; def\n(begin\n  (~macro X1 (1 %test-undefined-value))\n  1)\n")
              (test* #"an undefined variable in ~macro fails with its location"
                     (list #t #t #"~(car r):3" #t)
                     (let1 e (cdr r)
                       (list (sxmml-form-error? e)
                             (boolean (#/^\S+:3: / (condition-message e)))
                             (sxmml-form-error-location e)
                             (is-a? (sxmml-form-error-cause e) <unbound-variable-error>)))))
            (test* #"~macro without a location registers"
                   '(5 6)
                   (parameterize ([sxmml-environment (make <sxmml-environment>)])
                     (eval `(,macro X2 (5 6)) (interaction-environment))
                     (slot-ref (hash-table-get (slot-ref (sxmml-environment) table) 'X2) 'value)))
            (let1 source (write-source #"~|macro|-sxlc-location" #"(mml G \"c8\")\n(~macro X1)\n")
              (test* #"sxlc shows the location of a wrong ~macro"
                     '(1 #t)
                     (status-and-message (sxlc-status "-m" "pmd" "-o" (work-file #"~|macro|-sxlc-location.mml") source)
                                         (string->regexp
                                          (regexp-quote #"*** SXMML-FORM-ERROR: ~|source|:2: invalid ~macro form")))))))
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
        ;; memo は文字列 1 つなら 1 行、リストなら要素ごとに 1 行で出力すること (#87)
        (dolist [case '(("memo-string" "abc" ("#memo        abc"))
                        ("memo-list" ("m1" "m2") ("#memo        m1" "#memo        m2")))]
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
        ;; sxlc の -O 3 と -O 4 (#69)。標準出力の Optimize level の行と、生成物のトラック A の行
        (let1 converted (^[name text level]
                          (let1 lines (run-sxlc name `((mml A ,text)) "-m" "pmd" "-O" level)
                            (list (find #/^Optimize level:/ lines)
                                  (find #/^A / (file->string-list (work-file #"~|name|.mml")
                                                                  :encoding "shift_jis")))))
          (test* "-O 3 converts with the repeat optimization"
                 (list "Optimize level: 3" (cadr (converted "cli-o2-hand" "[cd:e]3" "2")))
                 (converted "cli-o3" "cdecdecd" "3"))
          (dolist (level '("4" "9"))
            (test* #"-O ~level expands small repeats"
                   (list #"Optimize level: ~level" "A l%24[o4a]16")
                   (converted #"cli-o~level" "[aaaaaaaa]2" level)))
          (test* "-O 2 is unchanged"
                 '("Optimize level: 2" "A l%24[o4aaaaaaaa]2")
                 (converted "cli-o2" "[aaaaaaaa]2" "2")))
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
                          ("[cdcdcdLcdcdcd]2" "[[o4cd]3L[cd]3]2")
                          ("[cdcdcd]0" "[[o4cd]3]0")
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
          ;; L を含むブロックと、ブロックの 2 個目以降の : は区切りとし、生成するループに入れない。
          ;; L を含むブロックは展開しない
          (dolist (case '(("cdefgab[dLe]2cdefgab" :repeat) ("[c:d:d:d:d]2" :repeat)
                          ("[cdLcd]2" :repeat-expand) ("[c:Ld]1" :repeat-expand)
                          ("[c:d:e]2" :repeat-expand) ("[c:d:d:d:d]2" :repeat-expand)))
            (test* #"\"~(car case)\" with optimize ~(cadr case) keeps L and : out of generated loops"
                   (optimized-line "repeat-hand" :full `((mml A ,(car case))))
                   (optimized-line "repeat" (cadr case) `((mml A ,(car case))))))
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
                          ("[cdcdcd]0" "[[o4cd]3]0")
                          ("[cdcdcd]1" "[o4cd]3")))
            (test* #"\"~(car case)\" with optimize :repeat-expand"
                   #"A l%24~(cadr case)"
                   (optimized-line "repeat" :repeat-expand `((mml A ,(car case))))))
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
          (dolist (text '("[aaaaaaaa]2" "[ab]2ab" "cdecdecd" "cdcdcdLcdcdcd" "[cdcdcdLcdcdcd]2"
                          "[cdcdcd]0" "c4c4c4c4c8" "[c:]2" "v10cdv12cdv10cdv12cd" "[c&c&]4"))
            (dolist (level '(:repeat :repeat-expand))
              (test* #"\"~|text|\" keeps its expansion with optimize ~level" #t
                     (let ([a (expansion (structure (optimized-objects 'A :full `((mml A ,text)))))]
                           [b (expansion (structure (optimized-objects 'A level `((mml A ,text)))))])
                       (and (= (length a) (length b)) (every same? a b)))))))
        ;; PCM の音色は 0 から 255、SSG の音色は 0 から 9 までの整数とし、
        ;; それ以外は書かれた値を示して失敗する (#58)
        (test* "a PCM voice 0 is accepted"
               0
               (car (sxlc-status "-m" "pmd" "-o" (work-file "voice-pcm-0.mml")
                                 (write-source "voice-pcm-0" "(mml J \"@0c8\")"))))
        (dolist (case '(("J" "256") ("J" "1.5") ("J" "x") ("G" "10") ("G" "1.5") ("G" "x")))
          (let1 source (write-source #"voice-~(car case)-~(cadr case)"
                                     #"(mml ~(car case) \"@{~(cadr case)}c8\")")
            (test* #"a voice ~(cadr case) on track ~(car case) fails"
                   '(1 #t)
                   (status-and-message (sxlc-status "-m" "pmd" "-o" (work-file "voice-invalid.mml") source)
                                       (string->regexp
                                        #"invalid voice parameter: +~(regexp-quote (cadr case))$")))))
        ;; 最適化で不正なチャンネルの命令が落ちず、-O 0 と同じく失敗する (#64)
        (let1 source (write-source "lfo-invalid-channel"
                                   "(mml A \"Mt{C} Mt c\")")
          (test* "an invalid LFO channel fails with optimize level 1"
                 '(1 #t)
                 (status-and-message (sxlc-status "-O" "1" "-m" "pmd" "-o" (work-file "lfo-invalid.mml") source)
                                     #/invalid lfo channel/)))
        ;; 終了値の体系 (#37)
        ;; 0 成功、1 変換の失敗、2 使用法の誤り、3 後処理の失敗
        (let1 ok (write-source "status-ok" "(mml G \"c8\")")
          (test* "a successful conversion exits with 0" 0
                 (car (sxlc-status "-m" "pmd" ok)))
          (test* "a successful post process exits with 0" 0
                 (car (sxlc-status "-m" "pmd" "-c" "true" ok)))
          (dolist [c `(("no source file" ())
                       ("an unknown option" ("-x" ,ok))
                       ("a non-integer optimize level" ("-O" "abc" "-m" "pmd" ,ok))
                       ("a missing option argument" ("-m"))
                       ("an unknown mode" ("-m" "nosuch" ,ok))
                       ("a mode that cannot be called" ("-m" "car" ,ok)))]
            (test* #"~(car c) is a usage error with 2" 2
                   (car (apply sxlc-status (cadr c)))))
          ;; -c の字句の誤りは変換の前に検出し、生成物を作らない
          (let1 source (write-source "status-bad-c" "(mml G \"c8\")")
            (test* "a malformed -c is a usage error with 2 before conversion" '(2 #f)
                   (list (car (sxlc-status "-m" "pmd" "-c" "echo 'a" source))
                         (file-exists? (work-file "status-bad-c.mml"))))))
        (dolist [c '(("an MML error" "status-mml" "(mml G \"c0\")" ())
                     ("a read error" "status-read" "(mml G \"c8\"" ())
                     ("a missing mode" "status-no-mode" "(mml G \"c8\")" #f)
                     ("an output error" "status-output" "(mml G \"c8\")" ("-o" "/nonexistent-sxmml-dir/out.mml")))]
          (test* #"~(car c) is a conversion failure with 1" 1
                 (car (apply sxlc-status
                             `(,@(if (cadddr c) '("-m" "pmd") '())
                               ,@(or (cadddr c) '())
                               ,(write-source (cadr c) (caddr c)))))))
        ;; 後処理の失敗 (#32)
        ;; 後処理の失敗は 3 で終了し、標準エラー出力に通知し、生成物は残す
        (let1 source (write-source "post" "(mml G \"c8\")")
          (test* "a failed post process exits with 3 and keeps the output" '(3 #t #t)
                 (append (status-and-message (sxlc-status "-m" "pmd" "-c" "false" source)
                                             #/^post process failed: exit status 1$/)
                         (list (file-exists? (work-file "post.mml")))))
          (test* "the exit status of the post process is reported" '(3 #t)
                 (status-and-message (sxlc-status "-m" "pmd" "-c" "sh -c 'exit 7' sh" source)
                                     #/^post process failed: exit status 7$/))
          (test* "a post process killed by a signal exits with 3" '(3 #t)
                 (status-and-message (sxlc-status "-m" "pmd" "-c" "sh -c 'kill -TERM $$' sh" source)
                                     #/^post process killed by signal 15$/))
          (test* "a post process that cannot be started exits with 3" '(3 #t)
                 (status-and-message (sxlc-status "-m" "pmd" "-c" "no-such-sxmml-command" source)
                                     #/^post process failed: exit status 1$/))
          ;; wslpath がない環境でのみ確かめる
          (unless (find-file-in-paths "wslpath")
            (test* "a failure of wslpath with -w exits with 3" '(3 #t)
                   (status-and-message (sxlc-status "-w" "-m" "pmd" "-c" "true" source)
                                       #/^post process failed: wslpath/))))
        ;; 出力先が入力ファイルと同じ場合 (#40)
        ;; -o の指定でも既定の出力先でも、使用法の誤り 2 で終了し、ソースを変更しないこと
        (let* ([text "(mml G \"c8\")"]
               [check (^[source . args]
                        (append (status-and-message
                                 (apply sxlc-status `(,@args ,source))
                                 #/^output file is the same as the source file: /)
                                (list (equal? (file->string source) text))))])
          (let1 source (write-source "same" text)
            (test* "-o with the source file is a usage error" '(2 #t #t)
                   (check source "-m" "pmd" "-o" source)))
          (let1 source (write-source "same-spelling" text)
            (test* "-o with another spelling of the source file is a usage error" '(2 #t #t)
                   (check source "-m" "pmd" "-o" (string-append workdir "/./same-spelling.sxl"))))
          (let1 source (begin (make-directory* (work-file "real"))
                              (sys-symlink "real" (work-file "link"))
                              (rlet1 f (work-file "real/via-link.sxl")
                                (with-output-to-file f (^[] (display text)))))
            (test* "-o with the source file through a symbolic link is a usage error" '(2 #t #t)
                   (check source "-m" "pmd" "-o" (work-file "link/via-link.sxl"))))
          ;; 既定の出力先は、モードを -m とソースのどちらで指定しても同じく検査すること
          (let1 source (work-file "default-same.mml")
            (with-output-to-file source (^[] (display text)))
            (test* "the default output that is the source file is a usage error" '(2 #t #t)
                   (check source "-m" "pmd")))
          (let* ([mode-text #"(sxmml-mode pmd)~text"]
                 [source (work-file "source-mode-same.mml")])
            (with-output-to-file source (^[] (display mode-text)))
            (test* "the default output is checked with the mode in source" '(2 #t #t)
                   (list (car (sxlc-status source))
                         (boolean (any #/^output file is the same as the source file: /
                                       (cadr (sxlc-status source))))
                         (equal? (file->string source) mode-text)))))
        ;; 入力ファイルより後ろの引数 (#42)
        ;; 入力ファイルは 1 つに限り、後ろの引数は使用法の誤り 2 としてソースを読み込む前に
        ;; 検出し、生成物を作らないこと。オプションに見える引数には書く位置を通知すること
        (let ([source (write-source "extra-a" "(mml G \"c8\")")]
              [other (write-source "extra-b" "(mml G \"d8\")")]
              [outputs (map work-file '("extra-a.mml" "extra-b.mml" "extra-o.mml"))])
          (define (check pattern . args)
            (for-each (^f (when (file-exists? f) (sys-unlink f))) outputs)
            (append (status-and-message (apply sxlc-status args) pattern)
                    (list (any file-exists? outputs))))
          (test* "two source files are a usage error" '(2 #t #f)
                 (check #/^unexpected argument after the source file: / "-m" "pmd" source other))
          (test* "-o after the source file is a usage error" '(2 #t #f)
                 (check #/^options must be placed before the source file$/
                        "-m" "pmd" source "-o" (work-file "extra-o.mml")))
          (test* "-m after the source file is a usage error" '(2 #t #f)
                 (check #/^options must be placed before the source file$/ source "-m" "pmd")))
        ;; 後処理の出力の文字コード (#41)
        ;; -w なしでは後処理の出力を変換せずにそのまま通し、-w ありでは CP932 から変換する。
        ;; どちらも復号できない出力で異常終了しないこと
        (let ([source (write-source "post-output" "(mml G \"c8\")")]
              [utf8 "sh -c \"printf '\\346\\227\\245\\346\\234\\254\\350\\252\\236\\n'\" sh"]
              [cp932 "sh -c \"printf '\\203\\145\\203\\130\\203\\147\\n'\" sh"]
              [broken "sh -c \"printf '\\377\\376\\n'\" sh"]
              ;; 引数のパスをそのまま返す wslpath を先に見つかる場所に置く
              [fake-path (let1 dir (work-file "fake-bin")
                           (make-directory* dir)
                           (with-output-to-file (build-path dir "wslpath")
                             (^[] (display "#!/bin/sh\nprintf '%s\\n' \"$2\"\n")))
                           (sys-chmod (build-path dir "wslpath") #o755)
                           (list "env" #"PATH=~|dir|:~(sys-getenv \"PATH\")"))])
          (test* "UTF-8 output of the post process is passed through without -w" '(0 #t)
                 (let1 r (sxlc-run '() "-m" "pmd" "-c" utf8 source)
                   (list (car r) (boolean (member "日本語" (cadr r))))))
          (test* "undecodable output of the post process does not fail without -w" 0
                 (car (sxlc-run '() "-m" "pmd" "-c" broken source)))
          (test* "CP932 output of the post process is converted with -w" '(0 #t)
                 (let1 r (sxlc-run fake-path "-w" "-m" "pmd" "-c" cp932 source)
                   (list (car r) (boolean (member "テスト" (cadr r))))))
          (test* "undecodable output of the post process does not fail with -w" 0
                 (car (sxlc-run fake-path "-w" "-m" "pmd" "-c" broken source)))))
    (remove-directory* workdir)))

;; (test* "test-sxmml" "sxmml is working"
;;        (test-sxmml))

;; If you don't want `gosh' to exit with nonzero status even if
;; the test fails, pass #f to :exit-on-failure.
(test-end :exit-on-failure #t)
