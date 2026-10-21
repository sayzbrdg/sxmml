;;;
;;; テスト用の共通部品
;;;

(define-module test.helpers
  (use file.util)
  (export make-test-sxlc-command))
(select-module test.helpers)

;; CLI の動作は sxlc を別プロセスで起動して検査する。
;; 親の読み込みパスは子へ自動では渡らないため、同じ Gauche と検索パスを指定する。
;; 子の作業ディレクトリを変える検査でも同じ CLI・モジュールを使えるよう、
;; 起動前にパスを絶対化する。
(define (make-test-sxlc-command)
  (let1 specified (sys-getenv "SXMML_SXLC")
    (unless (and specified (not (string=? specified "")))
      (error "SXMML_SXLC に検査対象の sxlc を指定してください。"))
    (let* ([invoked (car (os-command-line))]
           [gosh (if (string-scan invoked "/")
                   invoked
                   (find-file-in-paths invoked :pred file-is-executable?))])
      ;; -I は検索パスの先頭に追加されるので、元の優先順を保つため逆順に渡す。
      `(,(sys-normalize-pathname gosh :absolute #t)
        ,@(map (^p (string-append "-I" (sys-normalize-pathname p :absolute #t)))
               (reverse *load-path*))
        ,(sys-normalize-pathname specified :absolute #t)))))
