;;;
;;; 最適化の水準ごとの生成物のサイズ、オブジェクト数、時間を測る
;;;
;;; 使い方（リポジトリのルートで）:
;;;   gosh -I. tools/measure-optimize.scm [-m モード] ソース.sxl [設定 ...]
;;;
;;; モードの既定は pmd。設定は O2、O3、O4 のいずれかに、:E=数 と :W=数 を付けられる。
;;;   E は -O 4 で展開する既存のリピートの長さの上限、W は圧縮の窓幅。既定は E が 256、
;;;   W が O3 で 256、O4 で 1024（sxlc の -O 3・-O 4 と同じ）。O2 では使わない。
;;;   例: O2 O3 O4 O4:E=128:W=256
;;; 設定を省略すると O2 O3 O4 を測る。
;;;
;;; 出力の列は、設定、生成物のバイト数、O2 との比（O2 を先に測ったときだけ）、全トラックの
;;; オブジェクト数の合計、生成の時間、プロセス全体の時間。設定ごとに別のプロセスで変換し、
;;; 生成物は一時ディレクトリに置いて、測り終えたら消す。
;;;

(use gauche.process)
(use gauche.parseopt)
(use file.util)
(use srfi.13)
(use sxmml)
(use sxmml.backend.pmd)
(use sxmml.backend.pmd-binary)

(define script (sys-normalize-pathname (current-load-path) :absolute #t))

(define default-configs '("O2" "O3" "O4"))

;; "O4:E=64:W=128" を (水準の値の文字列 E W) にする
(define (parse-config text)
  (let* ([parts (string-split text #\:)]
         [level (assoc-ref '(("O2" . ":full") ("O3" . ":repeat") ("O4" . ":repeat-expand"))
                           (car parts))]
         [values (map (^p (if-let1 m (#/^([EW])=([0-9]+)$/ p)
                            (cons (m 1) (string->number (m 2)))
                            (exit 2 "invalid config: ~a" text)))
                      (cdr parts))])
    (unless level (exit 2 "invalid config: ~a" text))
    (list level (assoc-ref values "E" 256)
          (assoc-ref values "W" (if (equal? (car parts) "O4") 1024 256)))))

;; 子プロセス: 一つの設定で変換し、結果を result に書く
(define (child mode level e w source result outdir)
  (let1 t0 (current-time)
    (parameterize ([sxmml-source-file source]
                   [sxmml-mode-fixed? #t])
      ((eval (string->symbol mode) (current-module)))
      (load source)
      (slot-set! (sxmml-environment) 'optimize (read-from-string level))
      (eval `(begin (set! %repeat-window ,w) (set! %repeat-expand-window ,w)
                    (set! %repeat-expand-limit ,e))
            (find-module 'sxmml.optimizer))
      ;; バックエンドは各トラックを一度ずつ最適化するので、その戻り値の長さを足し合わせる。
      ;; 最適化はオブジェクトを書き換えるので、別に呼び直して数えない
      (let ([out (build-path outdir (if (#/binary/ mode) "out.m" "out.mml"))]
            [objects 0]
            [optimize (with-module sxmml.optimizer sxmml-optimize-tree)])
        (eval `(set! sxmml-optimize-tree
                     ,(^ args (rlet1 lis (apply optimize args) (inc! objects (length lis)))))
              (find-module 'sxmml.optimizer))
        (slot-set! (sxmml-environment) 'output out)
        (let1 t1 (current-time)
          ((slot-ref (sxmml-environment) 'backend))
          (let1 t2 (current-time)
            (with-output-to-file result
              (^[] (format #t "~d ~d ~a ~a\n" (file-size out) objects
                           (- (time->seconds t2) (time->seconds t1))
                           (- (time->seconds t2) (time->seconds t0)))))))))))

(define (driver mode source configs)
  (let ([gosh (car (os-command-line))]
        [load-options (map (^p (string-append "-I" (sys-normalize-pathname p :absolute #t)))
                           (reverse *load-path*))]
        [source (sys-normalize-pathname source :absolute #t)])
    (for-each parse-config configs)      ; 測り始める前に設定の誤りを見つける
    (format #t "~16a ~10a ~8a ~8a ~10a ~10a\n" "config" "bytes" "vs O2" "objects" "generate" "total")
    (let loop ([rest configs] [base #f])
      (unless (null? rest)
        (let* ([config (parse-config (car rest))]
               [dir (sys-mkdtemp (build-path (temporary-directory) "measure-optimize"))]
               [result (build-path dir "result")]
               [t0 (current-time)])
          (receive (status total row)
              (unwind-protect
                  (let* ([p (run-process `(,gosh ,@load-options ,script "--child" ,mode
                                                 ,@(map x->string config) ,source ,result ,dir)
                                         :output :null :error :null :wait #t)]
                         [total (- (time->seconds (current-time)) (time->seconds t0))]
                         [status (sys-wait-exit-status (process-exit-status p))])
                    (values status total
                            (and (zero? status) (file-exists? result)
                                 (map string->number
                                      (string-split (string-trim-right (file->string result)) #\space)))))
                (remove-directory* dir))
          (if row
            (let1 base (or base (and (equal? (car rest) "O2") (car row)))
              (format #t "~16a ~10d ~8a ~8d ~10,2f ~10,2f\n" (car rest) (car row)
                      (if base (format #f "~,1f%" (* 100.0 (/ (car row) base))) "-")
                      (cadr row) (caddr row) total)
              (flush)
              (loop (cdr rest) base))
            (begin
              (format #t "~16a failed (exit status ~d)\n" (car rest) status)
              (flush)
              (loop (cdr rest) base)))))))))

(define (main args)
  (if (and (pair? (cdr args)) (equal? (cadr args) "--child"))
    (begin
      (apply (^[mode level e w source result outdir]
               (child mode level (string->number e) (string->number w) source result outdir))
             (cddr args))
      0)
    (let-args (cdr args) ([mode "m=s" "pmd"] . rest)
      (when (null? rest)
        (exit 2 "usage: gosh -I. tools/measure-optimize.scm [-m mode] source.sxl [config ...]"))
      (driver mode (car rest) (if (null? (cdr rest)) default-configs (cdr rest)))
      0)))
