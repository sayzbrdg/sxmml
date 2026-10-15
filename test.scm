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
(use sxmml)
(test-module 'sxmml)

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
        ;; begin 内の定義は同じ番号になる (#27)。その場合も出力は一致すること
        (let1 forms (append (map voice-form '(c b a) '(0 1 2))
                            `((begin ,(voice-form 'e 3) ,(voice-form 'd 4))
                              (mml A "@{a}c8@{e}c8")))
          (test* "the same input yields byte-identical output"
                 (generated-text "reproducible" forms)
                 (generated-text "reproducible" forms))))
    (remove-directory* workdir)))

;; (test* "test-sxmml" "sxmml is working"
;;        (test-sxmml))

;; If you don't want `gosh' to exit with nonzero status even if
;; the test fails, pass #f to :exit-on-failure.
(test-end :exit-on-failure #t)
