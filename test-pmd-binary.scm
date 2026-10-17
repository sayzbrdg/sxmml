;;;
;;; Test sxmml_pmd_binary
;;;

(use gauche.test)

(test-start "sxmml_pmd_binary")
(use sxmml.backend.pmd-binary)
(test-module 'sxmml.backend.pmd-binary)
(use sxmml)
(use gauche.process)
(use gauche.uvector)
(use file.util)

(let* ([sxlc (or (sys-getenv "SXMML_SXLC")
                 (find-file-in-paths "sxlc" :paths (string-split (sys-getenv "PATH") #\:))
                 (error "sxlc が見つかりません。SXMML_SXLC にテスト対象の CLI を指定してください。"))]
       [gosh (car (os-command-line))]
       ;; 子プロセスでも、テスト本体と同じ sxmml と当バックエンドを使う。
       [load-options (map (^p (string-append "-I" (sys-normalize-pathname p :absolute #t)))
                          (reverse *load-path*))]
       [workdir (sys-mkdtemp (build-path (temporary-directory) "sxmml-pmd-binary-test"))])
  (define (work-file name) (build-path workdir name))
  (define (write-source path forms)
    (with-output-to-file path (^[] (for-each write forms)))
    path)
  (define (run-sxlc source . options)
    (let* ([status 0]
           [lines (process-output->string-list
                   `(,gosh ,@load-options ,sxlc ,@options ,source)
                   :error :merge
                   :on-abnormal-exit (^p (set! status (process-exit-status p))))])
      (list (sys-wait-exit-status status) lines)))
  (define (output-report result)
    (list (car result)
          (and-let1 m (any #/^Translate: .* => (.*)$/ (cadr result)) (m 1))
          (and-let1 m (any #/^<(.*)>$/ (cadr result)) (m 1))))
  (define post-command "printf '<%s>\\n'")
  ;; AL と FB の合成値で、出力された音色を識別する。
  (define (voice-form id marker)
    `(define-opn-voice ,id (,(modulo marker 8) ,(quotient marker 8))
       ,@(make-list 4 '(0 0 0 0 0 0 0 0 0))))
  ;; 先頭の 0 バイトに続くヘッダの 24・25 バイト目が音色表の位置。
  ;; 各音色は番号 1 バイト、音色データ 25 バイト（最後が AL/FB）。
  (define (voice-records path pmd3?)
    (let* ([data (file->bytevector path)]
           [start (+ 1 (u8vector-ref data 25) (ash (u8vector-ref data 26) 8))])
      (let loop ([offset start] [records '()])
        (if (if pmd3?
              (= offset (u8vector-length data))
              (and (= (u8vector-ref data offset) 0)
                   (= (u8vector-ref data (+ offset 1)) #xff)))
          (reverse records)
          (loop (+ offset 26)
                (cons (list (u8vector-ref data offset) (u8vector-ref data (+ offset 25)))
                      records))))))
  ;; ファイル名と内容を比較して、既存ファイルの変更や一時ファイルの残留を検出する。
  (define (snapshot dir)
    (map (^[name] (let1 path (build-path dir name)
                  (cons name (if (file-is-directory? path)
                               (snapshot path)
                               (file->bytevector path)))))
         (sort (directory-list dir :children? #t) string<?)))
  (unwind-protect
      (begin
        (unless (file-is-readable? sxlc)
          (error "テスト対象の CLI を読み込めません。SXMML_SXLC を確認してください。" sxlc))
        (dolist [mode '(pmd-binary pmd3-binary pmd4-binary)]
          (let* ([name (symbol->string mode)]
                 [source (write-source (work-file #"~|name|.sxl") '((mml G "c8")))]
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
                (write-source input `((sxmml-mode ,mode) (mml G "c8")))
                (let* ([before (snapshot dir)]
                       [result (apply run-sxlc input (if cli? (list "-m" name) '()))])
                  (test* #"~mode は入力と既定出力が同じ場合に上書きしない（-m: ~cli?）"
                         '(2 #t #t)
                         (list (car result)
                               (boolean (any #/^output file is the same as the source file:/
                                             (cadr result)))
                               (equal? before (snapshot dir)))))))))

        (let* ([source (write-source (work-file "metadata.sxl")
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
        (let* ([source (write-source (work-file "slot.sxl")
                                    `((slot-set! (sxmml-environment) 'output ,(work-file "source-slot.m"))
                                      (mml G "c8")))]
               [output (work-file "slot.m")])
          (test* "ソース中の output の変更より CLI の出力先決定を優先する"
                 (list 0 output output #t #f)
                 (append (output-report (run-sxlc source "-m" "pmd-binary" "-c" post-command))
                         (list (file-exists? output) (file-exists? (work-file "source-slot.m"))))))
        (let* ([source (write-source (work-file "switch.sxl")
                                    '((sxmml-mode pmd) (sxmml-mode pmd-binary) (mml G "c8")))]
               [output (work-file "switch.m")])
          (test* "ソースモードからバイナリモードへ切り替えても .m を生成する"
                 (list 0 output output #t #f)
                 (append (output-report (run-sxlc source "-c" post-command))
                         (list (file-exists? output) (file-exists? (work-file "switch.mml"))))))

        (test* "入力名なしのライブラリ利用では ./a.m を既定値として記録する"
               '("./a.m" "./a.m")
               (parameterize ([sxmml-environment (make <sxmml-environment>)]
                              [sxmml-source-file #f])
                 (pmd-binary)
                 (list ((with-module sxmml.backend.pmd-binary %output-file-get))
                       (slot-ref (sxmml-environment) 'output))))
        (let1 source (write-source (work-file "library.sxl") '((mml G "c8")))
          (test* "ライブラリから直接生成しても入力名に対応する .m を使う"
                 (list (work-file "library.m") #t)
                 (parameterize ([sxmml-environment (make <sxmml-environment>)]
                                [sxmml-source-file source])
                   (pmd-binary)
                   (load source)
                   (sxmml-pmd-binary-generate)
                   (list (slot-ref (sxmml-environment) 'output)
                         (file-exists? (work-file "library.m"))))))

        ;; 公開生成関数は、モード手続きを呼ばずにトラックを登録しても使える。
        (dolist [case (list (list "pmd-binary" sxmml-pmd-binary-generate)
                           (list "pmd3-binary" sxmml-pmd3-binary-generate)
                           (list "pmd4-binary" sxmml-pmd4-binary-generate))]
          (let* ([name (car case)]
                 [source (write-source (work-file #"direct-~|name|.sxl") '((mml G "c8")))]
                 [output (work-file #"direct-~|name|.m")])
            (test* #"~name はモード手続きなしの直接生成でも .m を使う"
                   (list output #t)
                   (parameterize ([sxmml-environment (make <sxmml-environment>)]
                                  [sxmml-source-file source])
                     (define-track (A B C D E F G H I J L))
                     (load source)
                     ((cadr case))
                     (list (slot-ref (sxmml-environment) 'output) (file-exists? output))))))

        (let* ([source (write-source (work-file "priority.sxl")
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

        (dolist [mode '("pmd-binary" "pmd3-binary" "pmd4-binary")]
          (let* ([source (write-source (work-file #"voices-~|mode|.sxl")
                                      `(,(voice-form 'z 3) ,(voice-form 'a 1)
                                        ,(voice-form 'm 2) ,(voice-form 'unused 7)
                                        (mml A "@{m}c8@{z}c8@{a}c8")))]
                 [output (work-file #"voices-~|mode|.m")])
            (test* #"~mode は使用音色だけを番号順に出力する"
                   '(0 ((0 3) (1 1) (2 2)))
                   (list (car (run-sxlc source "-m" mode "-o" output))
                         (and (file-exists? output)
                              (voice-records output (string=? mode "pmd3-binary")))))
            ;; begin 内の番号重複は上流の既知の問題。ここでは出力順だけを固定する。
            (write-source source `((begin ,(voice-form 'z 3) ,(voice-form 'a 0)
                                          ,(voice-form 'y 2) ,(voice-form 'b 1))
                                   (mml A "@{z}c8@{y}c8@{b}c8@{a}c8")))
            (test* #"~mode は同じ番号の音色を ID 順に出力する"
                   '(0 ((0 0) (0 1) (0 2) (0 3)))
                   (list (car (run-sxlc source "-m" mode "-o" output))
                         (voice-records output (string=? mode "pmd3-binary"))))
            (let1 before (file->bytevector output)
              (test* #"~mode の同じ入力は別プロセスでもバイト単位で一致する"
                     '(0 #t)
                     (list (car (run-sxlc source "-m" mode "-o" output))
                           (equal? before (file->bytevector output)))))))

        (dolist [case '(("read" "(mml G \"c8\"" "out.m" "読み込み")
                       ("generate" "(mml A \"@{undefined}c8\")" "out.m" "生成")
                       ("missing-dir" "(mml G \"c8\")" "missing/out.m" "出力先の不存在")
                       ("directory" "(mml G \"c8\")" "adir" "出力先への rename"))]
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
        (let* ([source (write-source (work-file "post-failure.sxl") '((mml G "c8")))]
               [result (run-sxlc source "-m" "pmd-binary" "-c" "false")])
          (test* "後処理の失敗は終了値 3 となり生成物は残る"
                 '(3 #t)
                 (list (car result) (file-exists? (work-file "post-failure.m"))))))
    (remove-directory* workdir)))

;; If you don't want `gosh' to exit with nonzero status even if
;; the test fails, pass #f to :exit-on-failure.
(test-end :exit-on-failure #t)
