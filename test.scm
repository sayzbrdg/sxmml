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
                         (list (file-exists? default) (file-exists? slot-out))))))
    (remove-directory* workdir)))

;; (test* "test-sxmml" "sxmml is working"
;;        (test-sxmml))

;; If you don't want `gosh' to exit with nonzero status even if
;; the test fails, pass #f to :exit-on-failure.
(test-end :exit-on-failure #t)
