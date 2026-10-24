;;;
;;; sxlcの検査
;;;

(use gauche.test)

(test-start "sxlc")
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
  ;; name.sxl に forms を書き出し、options を付けて sxlc を実行して出力行を返す
  (define (run-sxlc name forms . options)
    (let1 source (build-path workdir #"~|name|.sxl")
      (with-output-to-file source (^[] (for-each write forms)))
      (process-output->string-list
       `(,@sxlc-command ,@options ,source)
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
  ;; name.sxl に text を書いて返す
  (define (source-output name text)
    (rlet1 source (work-file #"~|name|.sxl")
      (with-output-to-file source (^[] (display text)))))
  ;; args を付けて sxlc を実行し、終了値と標準エラー出力の行のリストを返す
  (define (sxlc-status . args)
    (let* ([err (work-file "status.err")]
           [p (run-process `(,@sxlc-command ,@args)
                           :output :null :error err :wait #t)])
      (list (sys-wait-exit-status (process-exit-status p)) (file->string-list err))))
  ;; prefix のコマンドを前に付けて args で sxlc を実行し、終了値と、標準出力と
  ;; 標準エラー出力の行のリストを返す。出力は復号できないバイト列もそのまま読む
  (define (sxlc-run prefix . args)
    (let* ([status 0]
           [lines (process-output->string-list
                   `(,@prefix ,@sxlc-command ,@args)
                   :error :merge
                   :on-abnormal-exit (^p (set! status (process-exit-status p))))])
      (list (sys-wait-exit-status status) lines)))
  ;; sxlc-status の結果から、終了値と、pattern に一致する標準エラー出力の行の有無を返す
  (define (status-and-message result pattern)
    (list (car result) (boolean (any pattern (cadr result)))))
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
               (list #"warning: ~(work-file \"cli-mode.sxl\"):1: sxmml-mode in source is ignored; mode is already set to PMD"
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
                              `(,@sxlc-command
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
        (let ([slot-out (work-file "slot.mml")]
              [default (work-file "slot-set.mml")])
          (test* "the output slot set in source is overridden by sxlc"
                 (list default default #f #t #f)
                 (append (run-output-check "slot-set"
                                           `((slot-set! (sxmml-environment) 'output ,slot-out)
                                             (mml G "c8")))
                         (list (file-exists? default) (file-exists? slot-out)))))
        (let* (
          [op0 "(0 0 0 0 0 0 0 0 0)"]
          [voice-text (^[alfb op2] #"; voice\n(begin\n  (define-opn-voice V1 ~alfb\n    ~op0 ~op2 ~op0 ~op0)\n  (define %test-after-voice 1))\n")]
          )
          (let1 source (source-output "voice-sxlc-location"
                                     (string-append "(mml G \"c8\")\n" (voice-text "(0 0)" "(-1 0 0 0 0 0 0 0 0)")))
            (test* "sxlc shows the location of a wrong voice"
                   '(1 #t)
                   (status-and-message (sxlc-status "-m" "pmd" "-o" (work-file "voice-sxlc-location.mml") source)
                                       (string->regexp
                                        (regexp-quote #"*** SXMML-FORM-ERROR: ~|source|:4: invalid voice value"))))))
        (dolist [def '((define-lfo lfo) (define-envelope envelope))]
          (let ([macro (car def)])
            (let1 source (source-output #"~|macro|-sxlc-location" #"(mml G \"c8\")\n(~macro X1)\n")
              (test* #"sxlc shows the location of a wrong ~macro"
                     '(1 #t)
                     (status-and-message (sxlc-status "-m" "pmd" "-o" (work-file #"~|macro|-sxlc-location.mml") source)
                                         (string->regexp
                                          (regexp-quote #"*** SXMML-FORM-ERROR: ~|source|:2: invalid ~macro form")))))))
        (let1 source (source-output "processor-sxlc-location" "(mml G \"c8\")\n(mml-delay G (0 2 1) \"c8r8\")\n")
          (test* "sxlc shows the location of a wrong mml-delay value"
                 '(1 #t)
                 (status-and-message (sxlc-status "-m" "pmd" "-o" (work-file "processor-sxlc-location.mml") source)
                                     (string->regexp
                                      (regexp-quote #"*** SXMML-FORM-ERROR: ~|source|:2: invalid mml-delay value: ticks is 0")))))
        (let1 source (source-output "meta-sxlc-old" "(define-metadata memo (\"a\"))\n(mml G \"c8\")\n")
          (test* "sxlc warns about the old define-metadata form"
                 '(0 #t)
                 (status-and-message (sxlc-status "-m" "pmd" "-o" (work-file "meta-sxlc-old.mml") source)
                                     (string->regexp
                                      (regexp-quote #"warning: ~|source|:1: (define-metadata memo")))))
        (let1 source (source-output "meta-sxlc-wrong" "(mml G \"c8\")\n(define-metadata title)\n")
          (test* "sxlc shows the location of a wrong define-metadata"
                 '(1 #t)
                 (status-and-message (sxlc-status "-m" "pmd" "-o" (work-file "meta-sxlc-wrong.mml") source)
                                     (string->regexp
                                      (regexp-quote #"*** SXMML-FORM-ERROR: ~|source|:2: invalid define-metadata form")))))
        ;; sxlc の -O 3 と -O 4 (#69)。標準出力の Optimize level の行と、生成物のトラック A の行
        (let1 converted (^[name text level]
                          (let1 lines (run-sxlc name `((mml A ,text)) "-m" "pmd" "-O" level)
                            (list (find #/^Optimize level:/ lines)
                                  (find #/^A / (file->string-list (work-file #"~|name|.mml")
                                                                  :encoding "shift_jis")))))
          (test* "-O 3 converts with the repeat optimization"
                 (list "Optimize level: 3" (cadr (converted "cli-o2-hand" "[cd:e]3" "2")))
                 (converted "cli-o3" "cdecdecd" "3"))
          (dolist (level '("4" "9"))
            (test* #"-O ~level expands small repeats"
                   (list #"Optimize level: ~level" "A l%24[o4a]16")
                   (converted #"cli-o~level" "[aaaaaaaa]2" level)))
          (test* "-O 2 is unchanged"
                 '("Optimize level: 2" "A l%24[o4aaaaaaaa]2")
                 (converted "cli-o2" "[aaaaaaaa]2" "2")))
        ;; 終了値の体系 (#37)
        ;; 0 成功、1 変換の失敗、2 使用法の誤り、3 後処理の失敗
        (let1 ok (source-output "status-ok" "(mml G \"c8\")")
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
          (let1 source (source-output "status-bad-c" "(mml G \"c8\")")
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
                               ,(source-output (cadr c) (caddr c)))))))
        ;; 後処理の失敗 (#32)
        ;; 後処理の失敗は 3 で終了し、標準エラー出力に通知し、生成物は残す
        (let1 source (source-output "post" "(mml G \"c8\")")
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
          (let1 source (source-output "same" text)
            (test* "-o with the source file is a usage error" '(2 #t #t)
                   (check source "-m" "pmd" "-o" source)))
          (let1 source (source-output "same-spelling" text)
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
        (let ([source (source-output "extra-a" "(mml G \"c8\")")]
              [other (source-output "extra-b" "(mml G \"d8\")")]
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
        (let ([source (source-output "post-output" "(mml G \"c8\")")]
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
                 (car (sxlc-run fake-path "-w" "-m" "pmd" "-c" broken source))))
      )
    (remove-directory* workdir)))

(let* ([sxlc-command (make-test-sxlc-command)]
       [workdir (sys-mkdtemp (build-path (temporary-directory) "sxmml-test"))])
  (define (work-file name) (build-path workdir name))
  (define (source-output path forms)
    (with-output-to-file path (^[] (for-each write forms)))
    path)
  (define (run-sxlc source . options)
    (let* ([status 0]
           [lines (process-output->string-list
                   `(,@sxlc-command ,@options ,source)
                   :error :merge
                   :on-abnormal-exit (^p (set! status (process-exit-status p))))])
      (list (sys-wait-exit-status status) lines)))
  (define (output-report result)
    (list (car result)
          (and-let1 m (any #/^Translate: .* => (.*)$/ (cadr result)) (m 1))
          (and-let1 m (any #/^<(.*)>$/ (cadr result)) (m 1))))
  (define post-command "printf '<%s>\\n'")
  ;; ファイル名と内容を比較して、既存ファイルの変更や一時ファイルの残留を検出する。
  (define (snapshot dir)
    (map (^[name] (let1 path (build-path dir name)
                  (cons name (if (file-is-directory? path)
                               (snapshot path)
                               (file->bytevector path)))))
         (sort (directory-list dir :children? #t) string<?)))
  (unwind-protect
      (begin
        (dolist [mode '(pmd-binary pmd3-binary pmd4-binary)]
          (let* ([name (symbol->string mode)]
                 [source (source-output (work-file #"~|name|.sxl") '((mml G "c8")))]
                 [output (work-file #"~|name|.m")])
            (test* #"~mode は既定の .m を生成して同じ名前を表示・後処理へ渡す"
                   (list 0 output output #t)
                   (append (output-report (run-sxlc source "-m" name "-c" post-command))
                           (list (file-exists? output))))
            (let1 explicit (work-file #"~|name| ; output.bin")
              (test* #"~mode は -o の名前を一引数で後処理へ渡す"
                     (list 0 explicit explicit #t)
                     (append (output-report (run-sxlc source "-m" name "-o" explicit
                                                     "-c" post-command))
                             (list (file-exists? explicit)))))
            ;; -m とソース中のモード指定の両方で、既定出力の衝突を検査する。
            (dolist [cli? '(#t #f)]
              (let* ([dir (work-file #"~|name|-same-~cli?")]
                     [input (build-path dir "input.m")])
                (make-directory* dir)
                (source-output input `((sxmml-mode ,mode) (mml G "c8")))
                (let* ([before (snapshot dir)]
                       [result (apply run-sxlc input (if cli? (list "-m" name) '()))])
                  (test* #"~mode は入力と既定出力が同じ場合に上書きしない（-m: ~cli?）"
                         '(2 #t #t)
                         (list (car result)
                               (boolean (any #/^output file is the same as the source file:/
                                             (cadr result)))
                               (equal? before (snapshot dir)))))))))
        (let* ([source (source-output (work-file "metadata.sxl")
                                    `((define-metadata filename ,(work-file "obsolete.m"))
                                      (mml G "c8")))]
               [output (work-file "metadata.m")]
               [result (run-sxlc source "-m" "pmd-binary" "-c" post-command)])
          (test* "filename は警告して無視し、既定の出力先を使う"
                 (list 0 output output #t #t #f)
                 (append (output-report result)
                         (list (boolean (any #/^warning: metadata 'filename' is ignored/
                                             (cadr result)))
                               (file-exists? output)
                               (file-exists? (work-file "obsolete.m"))))))
        (let* ([source (source-output (work-file "slot.sxl")
                                    `((slot-set! (sxmml-environment) 'output ,(work-file "source-slot.m"))
                                      (mml G "c8")))]
               [output (work-file "slot.m")])
          (test* "ソース中の output の変更より CLI の出力先決定を優先する"
                 (list 0 output output #t #f)
                 (append (output-report (run-sxlc source "-m" "pmd-binary" "-c" post-command))
                         (list (file-exists? output) (file-exists? (work-file "source-slot.m"))))))
        (let* ([source (source-output (work-file "switch.sxl")
                                    '((sxmml-mode pmd) (sxmml-mode pmd-binary) (mml G "c8")))]
               [output (work-file "switch.m")])
          (test* "ソースモードからバイナリモードへ切り替えても .m を生成する"
                 (list 0 output output #t #f)
                 (append (output-report (run-sxlc source "-c" post-command))
                         (list (file-exists? output) (file-exists? (work-file "switch.mml"))))))
        (let* ([source (source-output (work-file "priority.sxl")
                                    '((sxmml-mode pmd4-binary) (mml G "c8")))]
               [output (work-file "priority.m")]
               [result (run-sxlc source "-m" "pmd3-binary")])
          (test* "-m のバイナリモードはソース中の指定より優先される"
                 '(0 #t #t)
                 (list (car result)
                       (boolean (member "Processing Mode: pmd3-binary" (cadr result)))
                       (let1 other (work-file "pmd3-binary.m")
                         (and (file-exists? output) (file-exists? other)
                              (equal? (file->bytevector output) (file->bytevector other)))))))
        (let* ([source (source-output (work-file "post-failure.sxl") '((mml G "c8")))]
               [result (run-sxlc source "-m" "pmd-binary" "-c" "false")])
          (test* "後処理の失敗は終了値 3 となり生成物は残る"
                 '(3 #t)
                 (list (car result) (file-exists? (work-file "post-failure.m")))))
      )
    (remove-directory* workdir)))

(test-end :exit-on-failure #t)
