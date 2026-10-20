;;;
;;; テスト用の CLI 起動処理の検査
;;;

(use gauche.test)
(use gauche.process)
(use file.util)
(use srfi.1)
(use test.cli-support)

(test-start "test.cli-support")

;; 環境変数は、検査が失敗しても元に戻す。
(define (with-environment name value thunk)
  (let1 previous (sys-getenv name)
    (unwind-protect
        (begin
          (if value (sys-setenv name value #t) (sys-unsetenv name))
          (thunk))
      (if previous (sys-setenv name previous #t) (sys-unsetenv name)))))

(define (with-sxlc value thunk)
  (with-environment "SXMML_SXLC" value thunk))

(let* ([srcdir (sys-normalize-pathname (build-path (sys-dirname (current-load-path)) "..")
                                     :absolute #t)]
       ;; 親の作業場所を保ったまま、fixture を相対パスで指定する。
       [workdir (sys-mkdtemp (build-path (sys-getcwd) ".sxmml-cli-support"))]
       [relative-workdir (sys-basename workdir)]
       [cli (build-path workdir "CLI ; 空白.scm")]
       [preferred (build-path workdir "優先 ; module")]
       [shadow (build-path workdir "後順位 module")]
       [childdir (build-path workdir "子プロセス")]
       [fakebin (build-path workdir "別のコマンド")]
       [output (build-path workdir "結果.scm")]
       [errors (build-path workdir "エラー.txt")])
  (unwind-protect
      (begin
        (for-each make-directory* (list preferred shadow childdir fakebin))
        (dolist [entry `((,preferred 指定したモジュール) (,shadow 別のモジュール))]
          (with-output-to-file (build-path (car entry) "issue60-fixture.scm")
            (^[]
              (for-each write
                        `((define-module issue60-fixture (export fixture-value))
                          (select-module issue60-fixture)
                          (define fixture-value ',(cadr entry)))))))
        (with-output-to-file cli
          (^[] (for-each write
                         '((use issue60-fixture)
                           (write (list fixture-value *argv*))
                           (newline)
                           (flush)
                           (sys-exit 17)))))
        (dolist [name '("gosh" "sxlc")]
          (let1 path (build-path fakebin name)
            (with-output-to-file path (^[] (display "#!/bin/sh\nexit 99\n")))
            (sys-chmod path #o755)))

        ;; 指定の欠落や不正な指定を見逃す変更を検出する。
        (dolist [entry `(("未指定" #f)
                        ("空の指定" "")
                        ("存在しないファイル" ,(build-path workdir "存在しない.scm"))
                        ("ディレクトリ" ,workdir))]
          (test* #"CLI の~(car entry)は指定方法を示して失敗する"
                 (test-error <error> #/SXMML_SXLC=.*gosh/)
                 (with-sxlc (cadr entry) make-test-sxlc-command)))

        ;; 相対パス、読み込み順序、引数の境界、Gauche の選択を実行結果で確かめる。
        (let1 previous-load-path *load-path*
          (unwind-protect
              (begin
                 (set! *load-path* (append (map (cut build-path relative-workdir <>)
                                               '("優先 ; module" "後順位 module"))
                                          previous-load-path))
                 (let1 command (with-sxlc (build-path relative-workdir "CLI ; 空白.scm")
                                         make-test-sxlc-command)
                   (with-environment
                    "PATH" fakebin
                    (^[]
                      (let1 process (run-process `(,@command "空白 付き" ";")
                                                 :directory childdir
                                                 :output output :error errors :wait #t)
                        (test* "作業場所と PATH を変えても指定した CLI を同じ Gauche で起動する"
                               17 (sys-wait-exit-status (process-exit-status process)))
                        (test* "モジュールの優先順と空白を含む引数を維持する"
                               '(指定したモジュール ("空白 付き" ";"))
                               (car (file->sexp-list output)))
                        (test* "子プロセスの起動時にエラーがない"
                               "" (file->string errors)))))))
            (set! *load-path* previous-load-path)))

        ;; 各テスト本体が指定を無視して別の CLI を起動する変更を検出する。
        (dolist [entry '(("本体" "test/test-sxmml.scm")
                        ("pmd-binary" "test/test-pmd-binary.scm"))]
          (let* ([marker (build-path workdir #"~(car entry)-起動記録")]
                 [spy (build-path workdir #"~(car entry)-CLI.scm")])
            (with-output-to-file spy
              (^[] (for-each write
                             `((with-output-to-file ,marker (^[] (display "起動した")))
                               (sys-exit 23)))))
            (with-sxlc
             spy
             (^[]
               (let* ([command (make-test-sxlc-command)]
                      [process (run-process `(,@(drop-right command 1)
                                              ,(build-path srcdir (cadr entry)))
                                            :output :null :error :null :wait #t)])
                 (test* #"~(car entry)のテストは指定した CLI を使い、その失敗を検出する"
                        '(#t #t)
                        (list (file-exists? marker)
                              (not (zero? (process-exit-status process)))))))))))
    (remove-directory* workdir)))

(test-end :exit-on-failure #t)
