;;;
;;; テスト本体と同じ Gauche・モジュールで指定された CLI を起動する。
;;;

(define-module test.cli-support
  (use file.util)
  (export make-test-sxlc-command))
(select-module test.cli-support)

(define (make-test-sxlc-command)
  (define (invalid-cli reason)
    (errorf "~a。SXMML_SXLC=./sxlc gosh -I. test/test-sxmml.scm のように検査対象を指定してください。"
            reason))
  (let1 specified (sys-getenv "SXMML_SXLC")
    (unless (and specified (not (string=? specified "")))
      (invalid-cli "SXMML_SXLC が設定されていません"))
    (let1 cli (sys-normalize-pathname specified :absolute #t)
      (unless (and (eq? (file-type cli) 'regular) (file-is-readable? cli))
        (invalid-cli #"指定した CLI を読み込めません: ~cli"))
      (let* ([invoked (car (os-command-line))]
             [gosh (if (string-scan invoked "/")
                     invoked
                     (find-file-in-paths invoked :pred file-is-executable?))])
        (unless gosh
          (errorf "テスト本体を起動した Gauche が見つかりません: ~a" invoked))
        ;; -I は読み込みパスの先頭へ追加するので、逆順に渡す。
        `(,(sys-normalize-pathname gosh :absolute #t)
          ,@(map (^p (string-append "-I" (sys-normalize-pathname p :absolute #t)))
                 (reverse *load-path*))
          ,cli)))))
