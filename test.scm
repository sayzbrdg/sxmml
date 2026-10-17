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
(use sxmml.backend.pmd)
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
             [p (run-process `("gosh" ,#"-I~srcdir" ,(build-path srcdir "sxlc.in")
                               "-m" "pmd" "-o" ,output ,source)
                             :directory dir :output :null :error :null :wait #t)])
        (list (not (zero? (process-exit-status p)))
              (equal? before (snapshot))))))
  ;; 音色 id を定義する式。音色の値で出力を区別できるよう AL に al を使う
  (define (voice-form id al)
    `(define-opn-voice ,id (,al 0) ,@(make-list 4 '(0 0 0 0 0 0 0 0 0))))
  ;; name.sxl の変換結果の生成物の内容を返す
  (define (generated-text name forms)
    (run-sxlc name forms "-m" "pmd")
    (file->string (work-file #"~|name|.mml")))
  ;; name.sxl に text を書いて返す
  (define (write-source name text)
    (rlet1 source (work-file #"~|name|.sxl")
      (with-output-to-file source (^[] (display text)))))
  ;; args を付けて sxlc を実行し、終了値と標準エラー出力の行のリストを返す
  (define (sxlc-status . args)
    (let* ([err (work-file "status.err")]
           [p (run-process `("gosh" ,#"-I~srcdir" ,(build-path srcdir "sxlc.in") ,@args)
                           :output :null :error err :wait #t)])
      (list (sys-wait-exit-status (process-exit-status p)) (file->string-list err))))
  ;; prefix のコマンドを前に付けて args で sxlc を実行し、終了値と、標準出力と
  ;; 標準エラー出力の行のリストを返す。出力は復号できないバイト列もそのまま読む
  (define (sxlc-run prefix . args)
    (let* ([status 0]
           [lines (process-output->string-list
                   `(,@prefix "gosh" ,#"-I~srcdir" ,(build-path srcdir "sxlc.in") ,@args)
                   :error :merge
                   :on-abnormal-exit (^p (set! status (process-exit-status p))))])
      (list (sys-wait-exit-status status) lines)))
  ;; sxlc-status の結果から、終了値と、pattern に一致する標準エラー出力の行の有無を返す
  (define (status-and-message result pattern)
    (list (car result) (boolean (any pattern (cadr result)))))
  ;; forms を最適化の水準 level で PMD 向けに変換し、トラック A の行を返す。
  ;; 最適化はオブジェクトを書き換えるので、水準ごとに読み込み直す
  (define (optimized-line name level forms)
    (let1 source (work-file #"optimize-~|name|.sxl")
      (with-output-to-file source (^[] (for-each write forms)))
      (parameterize ([sxmml-environment (make <sxmml-environment>)]
                     [sxmml-source-file source])
        (pmd-source)
        (slot-set! (sxmml-environment) 'optimize level)
        (load source)
        ((slot-ref (sxmml-environment) 'backend))
        (find #/^A / (string-split (file->string (slot-ref (sxmml-environment) 'output))
                                   "\r\n")))))
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
                         (list (file-exists? default) (file-exists? slot-out)))))
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
        ;; 生成物の再現性 (#16)
        ;; 音色定義は番号順に出力し、同じ入力からはバイト単位で同じ生成物が得られること
        (test* "voice definitions are output in number order"
               '(0 1 2 3 4 5 6 7 8 9)
               (map (^m (string->number (m 1)))
                    (filter-map #/^@(\d+)/
                                (string-split (generated-text
                                               "voice-order"
                                               (append (map voice-form '(j i h g f e d c b a) (iota 10))
                                                       '((mml A "@{a}c8"))))
                                              "\r\n"))))
        ;; begin 内の定義は同じ番号になる (#27)。その場合も出力は一致すること
        (let1 forms (append (map voice-form '(c b a) '(0 1 2))
                            `((begin ,(voice-form 'e 3) ,(voice-form 'd 4))
                              (mml A "@{a}c8@{e}c8")))
          (test* "the same input yields byte-identical output"
                 (generated-text "reproducible" forms)
                 (generated-text "reproducible" forms)))
        ;; 負の音高も、オクターブと 0～11 の音名に分けて出力すること
        (test* "a negative note number is output with its octave and note name"
               "A l%24o1co-1b"
               (find #/^A / (string-split (generated-text "negative-note" '((mml A "o1c<<b")))
                                          "\r\n")))
        ;; 最適化は、チャンネルの違う LFO の命令を互いに上書きしない (#64)。
        ;; 同じチャンネルの冗長な命令は落とす。チャンネルの省略と A は別に扱う
        (let1 lfo-defs '((define-lfo L1 (1 2 -3 4)) (define-lfo L2 (5 6 7 8)))
          (dolist (case `((#t "Mf Mf{B} c" "*0*B0o4c")
                          (#t "Mf{B} Mf c" "*B0*0o4c")
                          (#t "Mt Mt{B} c" "*1*B1o4c")
                          (#t "M{L1 A} M{L1 B} c" "M1,2,-3,4MB1,2,-3,4o4c")
                          (#t "Mt Mf c" "*0o4c")
                          (#t "Mt Mf{A} c" "*1*0o4c")
                          (:full "Mf Mf{B} c" "*0*B0o4c")
                          (:full "M{L1 A} M{L1 B} c" "M1,2,-3,4MB1,2,-3,4o4c")
                          (:full "Mf c Mf{B} c" "*0o4c*B0c")
                          (:full "M{L1 A} c M{L1 B} c" "M1,2,-3,4o4cMB1,2,-3,4c")
                          (:full "Mt Mf{A} c" "*1*0o4c")
                          (:full "Mt c Mt c" "*1o4cc")
                          (:full "M{L1 A} c M{L2 B} c M{L1 A} c" "M1,2,-3,4o4cMB5,6,7,8cc")
                          (:full "[Mt c Mt{B} c]2" "[*1o4c*B1c]2")
                          (:full "Mt{B} [Mt c Mt{B} c]2" "*B1[*1o4cc]2")
                          (:full "Mt [c Mt c Mf{B} c]2" "*1[o4cc*B0c]2")
                          (:full "M{L1 A} [c M{L1 A} c M{L2 B} c]2" "M1,2,-3,4[o4ccMB5,6,7,8c]2")
                          (:full "Mt [Mt{B} c Mt c Mf c]2" "*1[*B1o4c*1c*0c]2")
                          (:full "Mt c [Mt c Mf c]2" "*1o4c[*1c*0c]2")))
            (test* #"LFO commands \"~(cadr case)\" with optimize ~(car case)"
                   #"A l%24~(caddr case)"
                   (optimized-line "lfo" (car case)
                                   `(,@lfo-defs (mml A ,(cadr case)))))))
        ;; 最適化で不正なチャンネルの命令が落ちず、-O 0 と同じく失敗する (#64)
        (let1 source (write-source "lfo-invalid-channel"
                                   "(mml A \"Mt{C} Mt c\")")
          (test* "an invalid LFO channel fails with optimize level 1"
                 '(1 #t)
                 (status-and-message (sxlc-status "-O" "1" "-m" "pmd" "-o" (work-file "lfo-invalid.mml") source)
                                     #/invalid lfo channel/)))
        ;; 終了値の体系 (#37)
        ;; 0 成功、1 変換の失敗、2 使用法の誤り、3 後処理の失敗
        (let1 ok (write-source "status-ok" "(mml G \"c8\")")
          (test* "a successful conversion exits with 0" 0
                 (car (sxlc-status "-m" "pmd" ok)))
          (test* "a successful post process exits with 0" 0
                 (car (sxlc-status "-m" "pmd" "-c" "true" ok)))
          (dolist [c `(("no source file" ())
                       ("an unknown option" ("-x" ,ok))
                       ("a non-integer optimize level" ("-O" "abc" "-m" "pmd" ,ok))
                       ("a missing option argument" ("-m"))
                       ("an unknown mode" ("-m" "nosuch" ,ok))
                       ("a mode that cannot be called" ("-m" "car" ,ok)))]
            (test* #"~(car c) is a usage error with 2" 2
                   (car (apply sxlc-status (cadr c)))))
          ;; -c の字句の誤りは変換の前に検出し、生成物を作らない
          (let1 source (write-source "status-bad-c" "(mml G \"c8\")")
            (test* "a malformed -c is a usage error with 2 before conversion" '(2 #f)
                   (list (car (sxlc-status "-m" "pmd" "-c" "echo 'a" source))
                         (file-exists? (work-file "status-bad-c.mml"))))))
        (dolist [c '(("an MML error" "status-mml" "(mml G \"c0\")" ())
                     ("a read error" "status-read" "(mml G \"c8\"" ())
                     ("a missing mode" "status-no-mode" "(mml G \"c8\")" #f)
                     ("an output error" "status-output" "(mml G \"c8\")" ("-o" "/nonexistent-sxmml-dir/out.mml")))]
          (test* #"~(car c) is a conversion failure with 1" 1
                 (car (apply sxlc-status
                             `(,@(if (cadddr c) '("-m" "pmd") '())
                               ,@(or (cadddr c) '())
                               ,(write-source (cadr c) (caddr c)))))))
        ;; 後処理の失敗 (#32)
        ;; 後処理の失敗は 3 で終了し、標準エラー出力に通知し、生成物は残す
        (let1 source (write-source "post" "(mml G \"c8\")")
          (test* "a failed post process exits with 3 and keeps the output" '(3 #t #t)
                 (append (status-and-message (sxlc-status "-m" "pmd" "-c" "false" source)
                                             #/^post process failed: exit status 1$/)
                         (list (file-exists? (work-file "post.mml")))))
          (test* "the exit status of the post process is reported" '(3 #t)
                 (status-and-message (sxlc-status "-m" "pmd" "-c" "sh -c 'exit 7' sh" source)
                                     #/^post process failed: exit status 7$/))
          (test* "a post process killed by a signal exits with 3" '(3 #t)
                 (status-and-message (sxlc-status "-m" "pmd" "-c" "sh -c 'kill -TERM $$' sh" source)
                                     #/^post process killed by signal 15$/))
          (test* "a post process that cannot be started exits with 3" '(3 #t)
                 (status-and-message (sxlc-status "-m" "pmd" "-c" "no-such-sxmml-command" source)
                                     #/^post process failed: exit status 1$/))
          ;; wslpath がない環境でのみ確かめる
          (unless (find-file-in-paths "wslpath")
            (test* "a failure of wslpath with -w exits with 3" '(3 #t)
                   (status-and-message (sxlc-status "-w" "-m" "pmd" "-c" "true" source)
                                       #/^post process failed: wslpath/))))
        ;; 出力先が入力ファイルと同じ場合 (#40)
        ;; -o の指定でも既定の出力先でも、使用法の誤り 2 で終了し、ソースを変更しないこと
        (let* ([text "(mml G \"c8\")"]
               [check (^[source . args]
                        (append (status-and-message
                                 (apply sxlc-status `(,@args ,source))
                                 #/^output file is the same as the source file: /)
                                (list (equal? (file->string source) text))))])
          (let1 source (write-source "same" text)
            (test* "-o with the source file is a usage error" '(2 #t #t)
                   (check source "-m" "pmd" "-o" source)))
          (let1 source (write-source "same-spelling" text)
            (test* "-o with another spelling of the source file is a usage error" '(2 #t #t)
                   (check source "-m" "pmd" "-o" (string-append workdir "/./same-spelling.sxl"))))
          (let1 source (begin (make-directory* (work-file "real"))
                              (sys-symlink "real" (work-file "link"))
                              (rlet1 f (work-file "real/via-link.sxl")
                                (with-output-to-file f (^[] (display text)))))
            (test* "-o with the source file through a symbolic link is a usage error" '(2 #t #t)
                   (check source "-m" "pmd" "-o" (work-file "link/via-link.sxl"))))
          ;; 既定の出力先は、モードを -m とソースのどちらで指定しても同じく検査すること
          (let1 source (work-file "default-same.mml")
            (with-output-to-file source (^[] (display text)))
            (test* "the default output that is the source file is a usage error" '(2 #t #t)
                   (check source "-m" "pmd")))
          (let* ([mode-text #"(sxmml-mode pmd)~text"]
                 [source (work-file "source-mode-same.mml")])
            (with-output-to-file source (^[] (display mode-text)))
            (test* "the default output is checked with the mode in source" '(2 #t #t)
                   (list (car (sxlc-status source))
                         (boolean (any #/^output file is the same as the source file: /
                                       (cadr (sxlc-status source))))
                         (equal? (file->string source) mode-text)))))
        ;; 入力ファイルより後ろの引数 (#42)
        ;; 入力ファイルは 1 つに限り、後ろの引数は使用法の誤り 2 としてソースを読み込む前に
        ;; 検出し、生成物を作らないこと。オプションに見える引数には書く位置を通知すること
        (let ([source (write-source "extra-a" "(mml G \"c8\")")]
              [other (write-source "extra-b" "(mml G \"d8\")")]
              [outputs (map work-file '("extra-a.mml" "extra-b.mml" "extra-o.mml"))])
          (define (check pattern . args)
            (for-each (^f (when (file-exists? f) (sys-unlink f))) outputs)
            (append (status-and-message (apply sxlc-status args) pattern)
                    (list (any file-exists? outputs))))
          (test* "two source files are a usage error" '(2 #t #f)
                 (check #/^unexpected argument after the source file: / "-m" "pmd" source other))
          (test* "-o after the source file is a usage error" '(2 #t #f)
                 (check #/^options must be placed before the source file$/
                        "-m" "pmd" source "-o" (work-file "extra-o.mml")))
          (test* "-m after the source file is a usage error" '(2 #t #f)
                 (check #/^options must be placed before the source file$/ source "-m" "pmd")))
        ;; 後処理の出力の文字コード (#41)
        ;; -w なしでは後処理の出力を変換せずにそのまま通し、-w ありでは CP932 から変換する。
        ;; どちらも復号できない出力で異常終了しないこと
        (let ([source (write-source "post-output" "(mml G \"c8\")")]
              [utf8 "sh -c \"printf '\\346\\227\\245\\346\\234\\254\\350\\252\\236\\n'\" sh"]
              [cp932 "sh -c \"printf '\\203\\145\\203\\130\\203\\147\\n'\" sh"]
              [broken "sh -c \"printf '\\377\\376\\n'\" sh"]
              ;; 引数のパスをそのまま返す wslpath を先に見つかる場所に置く
              [fake-path (let1 dir (work-file "fake-bin")
                           (make-directory* dir)
                           (with-output-to-file (build-path dir "wslpath")
                             (^[] (display "#!/bin/sh\nprintf '%s\\n' \"$2\"\n")))
                           (sys-chmod (build-path dir "wslpath") #o755)
                           (list "env" #"PATH=~|dir|:~(sys-getenv \"PATH\")"))])
          (test* "UTF-8 output of the post process is passed through without -w" '(0 #t)
                 (let1 r (sxlc-run '() "-m" "pmd" "-c" utf8 source)
                   (list (car r) (boolean (member "日本語" (cadr r))))))
          (test* "undecodable output of the post process does not fail without -w" 0
                 (car (sxlc-run '() "-m" "pmd" "-c" broken source)))
          (test* "CP932 output of the post process is converted with -w" '(0 #t)
                 (let1 r (sxlc-run fake-path "-w" "-m" "pmd" "-c" cp932 source)
                   (list (car r) (boolean (member "テスト" (cadr r))))))
          (test* "undecodable output of the post process does not fail with -w" 0
                 (car (sxlc-run fake-path "-w" "-m" "pmd" "-c" broken source)))))
    (remove-directory* workdir)))

;; (test* "test-sxmml" "sxmml is working"
;;        (test-sxmml))

;; If you don't want `gosh' to exit with nonzero status even if
;; the test fails, pass #f to :exit-on-failure.
(test-end :exit-on-failure #t)
