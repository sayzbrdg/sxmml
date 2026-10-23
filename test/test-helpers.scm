;;;
;;; 各テストが指定した CLI を使い、その失敗を検出することを確認する。
;;;

(use gauche.test)
(use gauche.process)
(use file.util)

(test-start "テスト対象の CLI の選択")

(let1 srcdir (sys-normalize-pathname
              (build-path (sys-dirname (current-load-path)) "..") :absolute #t)
  (define (check-test-cli-use script)
    (let* ([workdir (sys-mkdtemp (build-path (temporary-directory) "sxmml-test-cli"))]
           [cli (build-path workdir "cli.scm")]
           [marker (build-path workdir "起動記録")]
           [previous (sys-getenv "SXMML_SXLC")])
      (unwind-protect
          (begin
            (with-output-to-file cli
              (^[] (for-each write
                             `((with-output-to-file ,marker (^[] (display "起動した")))
                               (sys-exit 23)))))
            (sys-setenv "SXMML_SXLC" cli #t)
            (let1 process (run-process `(,(car (os-command-line)) ,#"-I~srcdir"
                                         ,(build-path srcdir script))
                                      :output :null :error :null :wait #t)
              (list (file-exists? marker)
                    (not (zero? (process-exit-status process))))))
        (if previous
          (sys-setenv "SXMML_SXLC" previous #t)
          (sys-unsetenv "SXMML_SXLC"))
        (remove-directory* workdir))))

  (test* "sxlc のテストは指定した CLI を使い、その失敗を検出する"
         '(#t #t)
         (check-test-cli-use "test/test-sxlc.scm"))
  (test* "pmd-binary のテストは指定した CLI を使い、その失敗を検出する"
         '(#t #t)
         (check-test-cli-use "test/test-pmd-binary.scm")))

(test-end :exit-on-failure #t)
