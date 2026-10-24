;;;
;;; 最適化の水準を比べる計測の道具（tools/measure-optimize.scm）の検査
;;;

(use gauche.test)
(use gauche.process)
(use file.util)

(test-start "tools/measure-optimize")

(let* ([tools (sys-normalize-pathname (sys-dirname (current-load-path)) :absolute #t)]
       [srcdir (sys-dirname tools)]
       [script (build-path tools "measure-optimize.scm")]
       [workdir (sys-mkdtemp (build-path (temporary-directory) "sxmml-measure-test"))]
       [tmpdir (rlet1 dir (build-path workdir "tmp") (make-directory* dir))])
  ;; name.sxl に text を書いて返す
  (define (source-output name text)
    (rlet1 source (build-path workdir #"~|name|.sxl")
      (with-output-to-file source (^[] (display text)))))
  ;; args で計測の道具を実行し、終了値と標準出力の行のリストを返す。一時ディレクトリは
  ;; tmpdir の下に作らせ、後に残らないことを確かめられるようにする
  (define (measure . args)
    (let* ([status 0]
           [lines (process-output->string-list
                   `("env" ,#"TMPDIR=~tmpdir" "gosh" ,#"-I~srcdir" ,script ,@args)
                   :error :null
                   :on-abnormal-exit (^p (set! status (process-exit-status p))))])
      (list (sys-wait-exit-status status) lines)))
  ;; 出力の行を、空白で区切った列のリストにする
  (define (columns line) (remove (cut equal? "" <>) (string-split line #/\s+/)))
  ;; 出力の見出しより後の行を、列のリストにする
  (define (rows result) (map columns (if (pair? (cadr result)) (cdr (cadr result)) '())))
  ;; 列のリストからオブジェクト数を取り出す
  (define (objects row) (string->number (list-ref row 3 "")))

  (let* ([source (source-output "repeat" "(mml A \"[aaaaaaaa]2 cdecdecd\")")]
         [result (measure "-m" "pmd" source)]
         [table (rows result)])
    (test* "the default configs are O2, O3 and O4" '(0 ("O2" "O3" "O4"))
           (list (car result) (map car table)))
    (test* "the header names the columns"
           '("config" "bytes" "vs" "O2" "objects" "generate" "total")
           (columns (if (pair? (cadr result)) (car (cadr result)) "")))
    (test* "O2 is the base of the ratio" "100.0%" (list-ref (list-ref table 0 '()) 2 #f))
    ;; O2 の [aaaaaaaa]2cdecdecd（18 個）は、O3 で [[a]8]2[cd:e]3（11 個）、
    ;; O4 で [a]16[cd:e]3（9 個）になる
    (test* "objects are the totals of the optimized tracks" '(18 11 9) (map objects table))
    ;; E=8 では展開後の長さ 16 の [aaaaaaaa]2 を展開しないので、O3 と同じ 11 個になる
    (test* "E changes the -O 4 settings" '(11) (map objects (rows (measure source "O4:E=8"))))
    (test* "no temporary directory is left" '() (directory-list tmpdir :children? #t)))

  (let1 source (source-output "window" "(mml A \"c4d8e16f4g8a16b4[cd]64c4d8e16f4g8a16b4[cd]64\")")
    ;; 窓幅 256 では外側の反復を取り逃がし、: と [cd]64 の 5 個が既定の 1024 より多い
    (test* "W changes the -O 4 settings" '(5)
           (let1 counts (map objects (rows (measure source "O4" "O4:W=256")))
             (and (= (length counts) 2) (every number? counts)
                  (list (- (cadr counts) (car counts)))))))

  (let1 source (source-output "bad" "(mml A \"c0\")")
    (test* "a failed conversion is reported in the row" #t
           (boolean (any #/^O2 +failed \(exit status [1-9][0-9]*\)$/ (cadr (measure source "O2")))))
    (test* "no temporary directory is left after a failure" '()
           (directory-list tmpdir :children? #t)))

  (test* "an invalid config is a usage error" 2
         (car (measure (source-output "plain" "(mml A \"c\")") "O5")))
  (test* "a missing source is a usage error" 2 (car (measure)))
  (remove-directory* workdir))

(test-end :exit-on-failure #t)
