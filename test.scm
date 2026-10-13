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

;; sxlc の -c 後処理へ渡す生成名
(use gauche.process)
(use file.util)
(let* ([srcdir (sys-normalize-pathname (sys-dirname (current-load-path)) :absolute #t)]
       [workdir (sys-mkdtemp (build-path (temporary-directory) "sxmml-test"))])
  (define (run-sxlc-c name)
    (let1 source (build-path workdir #"~|name|.sxl")
      (with-output-to-file source (^[] (write '(mml G "c8"))))
      (process-output->string-list
       (list "gosh" #"-I~srcdir" (build-path srcdir "sxlc.in")
             "-m" "pmd" "-c" "printf '<%s>\\n'" source))))
  (unwind-protect
      (dolist [name '("semi;name" "space name")]
        (test* #"-c receives \"~|name|.mml\" as one argument"
               #"<~(build-path workdir name).mml>"
               (find #/^</ (run-sxlc-c name))))
    (remove-directory* workdir)))

;; (test* "test-sxmml" "sxmml is working"
;;        (test-sxmml))

;; If you don't want `gosh' to exit with nonzero status even if
;; the test fails, pass #f to :exit-on-failure.
(test-end :exit-on-failure #t)
