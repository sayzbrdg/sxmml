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
                       (generated-mode-line "cli-mode")))))
    (remove-directory* workdir)))

;; (test* "test-sxmml" "sxmml is working"
;;        (test-sxmml))

;; If you don't want `gosh' to exit with nonzero status even if
;; the test fails, pass #f to :exit-on-failure.
(test-end :exit-on-failure #t)
