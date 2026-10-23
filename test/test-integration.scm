;;;
;;; sxmml全体の結合試験の検査
;;;

(use gauche.test)

(test-start "sxmml全体の結合試験")
(use sxmml)
(use sxmml.form)
(use sxmml.backend.pmd)
(use sxmml.backend.pmd-binary)
(use gauche.uvector)
(use gauche.process)
(use file.util)
(use test.helpers)

(let* ([sxlc-command (make-test-sxlc-command)]
       [workdir (sys-mkdtemp (build-path (temporary-directory) "sxmml-test"))])
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
             [p (run-process `(,@sxlc-command
                               "-m" "pmd" "-o" ,output ,source)
                             :directory dir :output :null :error :null :wait #t)])
        (list (not (zero? (process-exit-status p)))
              (equal? before (snapshot))))))
  ;; name.sxl に text を書いて返す
  (define (write-source name text)
    (rlet1 source (work-file #"~|name|.sxl")
      (with-output-to-file source (^[] (display text)))))
  ;; args を付けて sxlc を実行し、終了値と標準エラー出力の行のリストを返す
  (define (sxlc-status . args)
    (let* ([err (work-file "status.err")]
           [p (run-process `(,@sxlc-command ,@args)
                           :output :null :error err :wait #t)])
      (list (sys-wait-exit-status (process-exit-status p)) (file->string-list err))))
  ;; sxlc-status の結果から、終了値と、pattern に一致する標準エラー出力の行の有無を返す
  (define (status-and-message result pattern)
    (list (car result) (boolean (any pattern (cadr result)))))
  (unwind-protect
      (begin
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
        ;; どちらのバックエンドでも、0回は構文解釈時に拒否し、既存の生成物を保つ (#106)
        (dolist [mode '("pmd" "pmd-binary")]
          (let* ([source (write-source #"repeat-zero-~mode" "(mml A \"[a:b]0\")")]
                 [output (work-file #"repeat-zero-~|mode|.out")])
            (with-output-to-file output (^[] (display "既存の生成物")))
            (test* #"~mode は0回を回数の誤りとして終了値1で拒否し、既存の生成物を保つ"
                   '(1 #t "既存の生成物")
                   (append (status-and-message (sxlc-status "-m" mode "-o" output source)
                                               #/repeat count must be a positive exact integer: +0/)
                           (list (file->string output))))))
      )
    (remove-directory* workdir)))

(let* ([sxlc-command (make-test-sxlc-command)]
       [workdir (sys-mkdtemp (build-path (temporary-directory) "sxmml-test"))])
  (define (work-file name) (build-path workdir name))
  (define (run-sxlc source . options)
    (let* ([status 0]
           [lines (process-output->string-list
                   `(,@sxlc-command ,@options ,source)
                   :error :merge
                   :on-abnormal-exit (^p (set! status (process-exit-status p))))])
      (list (sys-wait-exit-status status) lines)))
  ;; ファイル名と内容を比較して、既存ファイルの変更や一時ファイルの残留を検出する。
  (define (snapshot dir)
    (map (^[name] (let1 path (build-path dir name)
                  (cons name (if (file-is-directory? path)
                               (snapshot path)
                               (file->bytevector path)))))
         (sort (directory-list dir :children? #t) string<?)))
  (unwind-protect
      (begin
        (dolist [case '(("read" "(mml G \"c8\"" "out.m" "読み込み")
                       ("generate" "(mml A \"@{undefined}c8\")" "out.m" "生成")
                       ("missing-dir" "(mml G \"c8\")" "missing/out.m" "出力先の不存在")
                       ("directory" "(mml G \"c8\")" "adir" "出力先への rename")
                       ("note-range" "(mml A \"o8c>c\")" "out.m" "音高の範囲外")
                       ("size-limit" "(mml A \"c%20000000\")" "out.m" "大きさの上限"))]
          (let* ([dir (work-file (car case))]
                 [source (build-path dir "input.sxl")])
            (make-directory* (build-path dir "adir"))
            (with-output-to-file source (^[] (display (cadr case))))
            (with-output-to-file (build-path dir "out.m") (^[] (display "既存の生成物")))
            (let* ([before (snapshot dir)]
                   [result (run-sxlc source "-m" "pmd-binary"
                                     "-o" (build-path dir (caddr case)))])
              (test* #"~(cadddr case)の失敗は既存ファイルとディレクトリの内容を変えない"
                     '(1 #t)
                     (list (car result) (equal? before (snapshot dir)))))))
      )
    (remove-directory* workdir)))

(test-end :exit-on-failure #t)
