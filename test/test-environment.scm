;;;
;;; sxmml.environmentの検査
;;;

(use gauche.test)

(test-start "sxmml.environment")
(use sxmml.environment)
(test-module 'sxmml.environment)
(use sxmml)
(use sxmml.form)
(use sxmml.backend.pmd)
(use gauche.process)
(use file.util)
(use test.helpers)

(let* ([workdir (sys-mkdtemp (build-path (temporary-directory) "sxmml-test"))])
  (define (work-file name) (build-path workdir name))
  ;; name.sxl に text を書いて返す
  (define (source-output name text)
    (rlet1 source (work-file #"~|name|.sxl")
      (with-output-to-file source (^[] (display text)))))
  (unwind-protect
      (begin
        (let ()
          (define (named-load label text table)
            (let1 source (source-output label text)
              (parameterize ([sxmml-environment (make <sxmml-environment>)])
                (let ([result #f])
                  (let1 warnings
                      (call-with-output-string
                       (^p (with-error-to-port p
                             (^[] (set! result
                                       (guard (e [else e])
                                         (load source)
                                         (slot-ref (sxmml-environment) table)))))))
                    (list source result warnings))))))
          (define (named-object r id)
            (and (hash-table? (cadr r)) (hash-table-get (cadr r) id #f)))
          (define (lfo-report r id)
            (and-let1 obj (named-object r id)
              (list (slot-ref obj 'id) (slot-ref obj 'type) (slot-ref obj 'value))))
          (define (name-error macro value expected)
            #"invalid ~|macro| name: ~|value| (expected ~|expected|)")
          (dolist [case '((define-lfo lfo triangle) (define-envelope envelope default))]
            (let* ([macro (car case)] [table (cadr case)] [type (caddr case)]
                   [r (named-load #"~|macro|-generated"
                        #"(let ([depth 7])\n (define-syntax inner-name (syntax-rules () [(_) (~|macro| FIXED ~|type| (1 2 depth 4))]))\n (define-syntax outer-name (syntax-rules () [(_) (inner-name)]))\n (outer-name))\n" table)])
              (test* #"利用者マクロから~|macro|の固定名と種類を登録する"
                     (list 'FIXED type '(1 2 7 4)) (lfo-report r 'FIXED))
              (test* #"~|macro|の数値名0をsymbolで登録する"
                     '(|0| default (1 2 3 4))
                     (lfo-report (named-load #"~|macro|-number" #"(~|macro| 0 (1 2 3 4))\n" table) '|0|))))
          (dolist [case `((define-lfo lfo "\"L1\"" #f "a symbol or number")
                          (define-envelope envelope "#t" #f "a symbol or number")
                          (define-lfo lfo "|a b|" #f "a non-empty name without whitespace or braces")
                          (define-envelope envelope "|a{b}|" #f "a non-empty name without whitespace or braces")
                          (define-lfo lfo "L1" "\"triangle\"" "a symbol"))]
            (let* ([macro (car case)] [table (cadr case)] [name (caddr case)]
                   [type (list-ref case 3)] [expected (list-ref case 4)]
                   [type-text (or type "")]
                   [r (named-load #"~|macro|-invalid-~|name|"
                        #"; name\n(begin\n (~|macro| ~|name| ~|type-text| (1 2 3 4))\n 1)\n" table)]
                   [e (cadr r)]
                   [shown (format #f "~s" (read-from-string (or type name)))])
              (test* #"~|macro|の不正な名前または種類を位置付きで拒否する: ~|name| ~|type|"
                     (list #t (if type
                                #"invalid ~|macro| type: ~|shown| (expected ~|expected|)"
                                (name-error macro shown expected)) #"~(car r):3")
                     (if (sxmml-error? e)
                       (list (sxmml-form-syntax-error? e) (condition-message e)
                             (and (sxmml-source-context? e) (sxmml-source-context-location e)))
                       '(registered)))))
          (let1 r (named-load "key-generated"
                    "(define-syntax generated-title (syntax-rules () [(_) (define-metadata TITLE \"song\")]))\n(generated-title)\n" 'metadata)
            (test* "利用者マクロのメタデータキーを小文字のsymbolにする"
                   "song" (named-object r 'title)))
          (let1 r (named-load "key-old-generated"
                    "; memo\n(define-syntax generated-memo (syntax-rules () [(_) (define-metadata MEMO (\"a\"))]))\n(generated-memo)\n" 'metadata)
            (test* "利用者マクロの旧メタデータ書式も正規化したキーで一度警告する"
                   (list '("a") (list #"warning: ~(car r):3: (define-metadata memo (\"...\" ...)) is deprecated; use (list \"...\" ...) or '(\"...\" ...)"))
                   (list (named-object r 'memo)
                         (filter #/^warning:/ (string-split (caddr r) "\n")))))
          (dolist [key '("0" "\"title\"" "#t" "(a b)")]
            (let* ([r (named-load #"key-invalid-~|key|"
                        #"; key\n(begin\n (define-metadata ~|key| \"song\")\n 1)\n" 'metadata)]
                   [e (cadr r)])
              (test* #"不正なメタデータキーを位置付きで拒否する: ~|key|"
                     (list #t #"invalid define-metadata key: ~|key| (expected a symbol)" #"~(car r):3")
                     (if (sxmml-error? e)
                       (list (sxmml-form-syntax-error? e) (condition-message e)
                             (and (sxmml-source-context? e) (sxmml-source-context-location e)))
                       '(registered)))))
          (let* ([r (named-load "track-generated"
                     "(define-syntax generated-track (syntax-rules () [(_) (define-track FIXED)]))\n(generated-track)\n" 'track)]
                 [ctx (named-object r 'FIXED)])
            (test* "利用者マクロの単一トラック名と既定partはsymbolになる"
                   '(FIXED FIXED)
                   (and ctx (list (slot-ref ctx 'track) (slot-ref ctx 'part)))))
          (let* ([r (named-load "tracks-generated"
                     "(define recorded-order '())\n(define-syntax generated-tracks (syntax-rules () [(_) (define-track (FIRST SECOND) :part (let1 exists (boolean (sxmml-track-get 'SECOND)) (set! recorded-order (cons exists recorded-order)) (if exists 'FIRST 'SECOND)))]))\n(generated-tracks)\n" 'track)]
                 [first (named-object r 'FIRST)] [second (named-object r 'SECOND)])
            (test* "利用者マクロの複数トラック名とパラメータの評価順を維持する"
                   '(FIRST SECOND FIRST SECOND (#f #t))
                   (and first second
                        (list (slot-ref first 'track) (slot-ref second 'track)
                              (slot-ref first 'part) (slot-ref second 'part)
                              (reverse (eval 'recorded-order (interaction-environment)))))))
          (dolist [case '(("0" "0") ("\"G\"" "\"G\"") ("(G 0)" "0"))]
            (let* ([name (car case)] [r (named-load #"track-invalid-~|name|"
                                        #"; track\n(begin\n (define-track ~|name|)\n 1)\n" 'track)]
                   [e (cadr r)])
              (test* #"不正なトラック名を位置付きで拒否する: ~|name|"
                     (list #t #"invalid define-track name: ~(cadr case) (expected a symbol)" #"~(car r):3")
                     (if (sxmml-error? e)
                       (list (sxmml-form-syntax-error? e) (condition-message e)
                             (and (sxmml-source-context? e) (sxmml-source-context-location e)))
                       '(registered)))))
          (let* ([r (named-load "track-space" "(define-track |a b|)\n" 'track)]
                 [ctx (named-object r '|a b|)])
            (test* "トラック名にはMML用定義名の文字制限を適用しない"
                   '|a b| (and ctx (slot-ref ctx 'track))))
          (test* "直接のLFO・エンベロープ登録とメタデータ設定は名前制限を追加しない"
                 '(|a b| |a b| "song")
                 (parameterize ([sxmml-environment (make <sxmml-environment>)])
                   (sxmml-lfo-set! '|a b| 'default '(1 2 3 4))
                   (sxmml-envelope-set! '|a b| 'default '(1 2 3 4))
                   (sxmml-metadata-set! '|a b| "song")
                   (list (slot-ref (sxmml-lfo-get '|a b|) 'id)
                         (slot-ref (sxmml-envelope-get '|a b|) 'id)
                         (sxmml-metadata-get '|a b|)))))
        (test* "解像度0は値エラーになる"
               (test-error <sxmml-value-error> "invalid resolution: 0 (expected a positive integer)")
               (parameterize ([sxmml-environment (make <sxmml-environment>)])
                 (sxmml-resolution 0)))
        (test* "空のdefine-trackはフォーム構文エラーになる"
               (test-error <sxmml-form-syntax-error> "define-track requires at least one track name")
               (eval '(define-track ()) (interaction-environment)))

        (test* "sxmml-output-file is not exported"
               #f
               (boolean (memq 'sxmml-output-file
                              (module-exports (find-module 'sxmml.environment)))))
        ;; define-lfo・define-envelope の枠の中の値は式として評価し、種類はリテラルとする。
        ;; 既知の入力エラーには定義位置を添え、値の式の未知の例外は通す (#28)
        (dolist [def '((define-lfo lfo) (define-envelope envelope))]
          (let* ([macro (car def)]
                 [table (cadr def)]
                 [load-result (^[name text]
                                (let1 source (source-output name text)
                                  (parameterize ([sxmml-environment (make <sxmml-environment>)]
                                                 [sxmml-source-file source])
                                    (guard (e [else (cons source e)])
                                      (load source)
                                      (cons source (hash-table-get (slot-ref (sxmml-environment) table) 'X1 #f))))))]
                 [registered (^[name text]
                               (let1 obj (cdr (load-result name text))
                                 (list (slot-ref obj 'type) (slot-ref obj 'value))))]
                 [form-message #"invalid ~|macro| form: expected (~|macro| id [type] (value ...))"])
            (test* #"elements of ~macro are evaluated"
                   '(default (1 2 3 4))
                   (registered #"~|macro|-evaluated"
                               #"(define %test-depth 3)\n(~macro X1 (1 2 %test-depth (+ 1 3)))\n"))
            (test* #"~macro takes a literal type"
                   '(sw (1 2 3 4))
                   (registered #"~|macro|-type" #"(define sw 1)\n(~macro X1 sw (1 2 3 4))\n"))
            (test* #"~macro accepts an empty frame"
                   '(default ())
                   (registered #"~|macro|-empty" #"(~macro X1 ())\n"))
            (dolist [case '(("no-frame" "") ("not-list" " 1") ("two-frames" " (1 2) (3 4)")
                            ("non-symbol-type" " 1 (1 2)") ("too-many" " sw (1 2) 3"))]
              (let1 r (load-result #"~|macro|-~(car case)" #"; def\n(begin\n  (~macro X1~(cadr case))\n  1)\n")
                (test* #"a wrong ~macro form fails with its location: ~(car case)"
                       (list #t (if (equal? (car case) "non-symbol-type")
                                     #"invalid ~macro type: 1 (expected a symbol)"
                                     form-message) #"~(car r):3" #t)
                       (let1 e (cdr r)
                         (list (sxmml-form-syntax-error? e) (condition-message e)
                               (sxmml-source-context-location e) (sxmml-form-syntax-error? e))))))
            (let1 r (load-result #"~|macro|-undefined" #"; def\n(begin\n  (~macro X1 (1 %test-undefined-value))\n  1)\n")
              (test* #"~|macro|内の未定義変数はGaucheのエラーとして通過する"
                     '(#t #f #f)
                     (let1 e (cdr r)
                       (list (condition-has-type? e <unbound-variable-error>)
                             (sxmml-error? e) (sxmml-source-context? e)))))
            (test* #"~macro without a location registers"
                   '(5 6)
                   (parameterize ([sxmml-environment (make <sxmml-environment>)])
                     (eval `(,macro X2 (5 6)) (interaction-environment))
                     (slot-ref (hash-table-get (slot-ref (sxmml-environment) table) 'X2) 'value)))
            ))
        ;; sxmml-mode の引数は式として評価する。-m でモードが決まっているときは、モード名も
        ;; 引数も評価せず、位置付きの警告だけを出す。誤りにはフォームの開始位置を付ける (#51)
        (let* ([mode-load (^[name text fixed]
                            (let1 source (source-output name text)
                              (parameterize ([sxmml-environment (make <sxmml-environment>)]
                                             [sxmml-source-file source]
                                             [sxmml-mode-fixed fixed])
                                (when fixed (pmd-source))
                                (let* ([result #f]
                                       [err (call-with-output-string
                                              (^p (with-error-to-port p
                                                    (^[] (set! result
                                                               (guard (e [else e])
                                                                 (load source)
                                                                 (slot-ref (sxmml-environment) 'mode)))))))])
                                  (list source result
                                        (filter #/^warning:/ (string-split err "\n")))))))]
               [error-report (^[r] (let1 e (cadr r)
                                     (list (sxmml-form-syntax-error? e) (condition-message e)
                                           (sxmml-source-context-location e))))]
               [ignored (^[location] #"warning: ~|location|: sxmml-mode in source is ignored; mode is already set to PMD")]
               ;; 3 行目の begin の中に form を置いた入力
               [in-begin (^[form] #"; mode\n(begin\n  ~form\n  1)\n")]
               [arg-mode "(define (%test-arg-mode n) (pmd) (slot-set! (sxmml-environment) 'mode (format #f \"PMD-~a\" n)))\n"])
          (let1 r (mode-load "mode-args" #"~|arg-mode|(sxmml-mode %test-arg-mode (+ 1 2))\n" #f)
            (test* "sxmml-mode evaluates its arguments"
                   '("PMD-3" ())
                   (cdr r)))
          (let1 r (mode-load "mode-fixed" (in-begin "(sxmml-mode %test-undefined-mode (car '()))") #t)
            (test* "sxmml-mode does not evaluate the mode and the arguments when the mode is fixed"
                   (list "PMD" (list (ignored #"~(car r):3")))
                   (cdr r)))
          (let1 r (mode-load "mode-fixed-args" #"~|arg-mode|(sxmml-mode %test-arg-mode (+ 1 2))\n" #t)
            (test* "sxmml-mode with arguments is ignored when the mode is fixed"
                   (list "PMD" (list (ignored #"~(car r):2")))
                   (cdr r)))
          (dolist [fixed '(#f #t)]
            (let1 r (mode-load #"mode-no-mode-~fixed" (in-begin "(sxmml-mode)") fixed)
              (test* #"a wrong sxmml-mode form fails at expansion with its location: fixed ~fixed"
                     (list #t #"invalid sxmml-mode form: expected (sxmml-mode mode arg ...)" #"~(car r):3" #t)
                     (append (error-report r) (list (sxmml-form-syntax-error? (cadr r)))))))
          (let1 r (mode-load "mode-undefined" (in-begin "(sxmml-mode %test-undefined-mode)") #f)
            (test* "未定義モードのSchemeエラーを変換しない"
                   '(#t #f #f)
                   (let1 e (cadr r)
                     (list (condition-has-type? e <unbound-variable-error>)
                             (sxmml-error? e) (sxmml-source-context? e)))))
          (let1 r (mode-load "mode-wrong-args" (in-begin "(sxmml-mode pmd 1)") #f)
            (test* "モード手続きの引数個数エラーを変換しない"
                   '(#t #f #f)
                   (let1 e (cadr r)
                     (list (condition-has-type? e <error>)
                           (sxmml-error? e) (sxmml-source-context? e)))))
          (test* "sxmml-mode without a location is ignored with a warning when the mode is fixed"
                 '("PMD" "warning: sxmml-mode in source is ignored; mode is already set to PMD\n")
                 (parameterize ([sxmml-environment (make <sxmml-environment>)]
                                [sxmml-mode-fixed #t])
                   (pmd-source)
                   (let1 err (call-with-output-string
                               (^p (with-error-to-port p
                                     (^[] (eval (list 'sxmml-mode 'pmd3) (interaction-environment))))))
                     (list (slot-ref (sxmml-environment) 'mode) err)))))
        ;; define-metadata の値は式として評価する。括弧の中が全て文字列の旧書式は、
        ;; 展開時に位置付きの警告を 1 回出して文字列のリストとして受け付ける (#13)
        (let* ([meta-load (^[name text :optional (after #f)]
                            (let1 source (source-output name text)
                              (parameterize ([sxmml-environment (make <sxmml-environment>)]
                                             [sxmml-source-file source])
                                (let* ([result #f]
                                       [err (call-with-output-string
                                              (^p (with-error-to-port p
                                                    (^[] (set! result
                                                               (guard (e [else e])
                                                                 (load source)
                                                                 (when after (after))
                                                                 (slot-ref (sxmml-environment) 'metadata)))))))])
                                  (list source result
                                        (filter #/^warning:/ (string-split err "\n")))))))]
               [value-of (^[r key] (hash-table-get (cadr r) key #f))]
               [old-warning (^[location key]
                              #"warning: ~|location|(define-metadata ~key (\"...\" ...)) is deprecated; use (list \"...\" ...) or '(\"...\" ...)")])
          (dolist [case '(("variable" "(define %test-title \"曲\")\n(define-metadata title %test-title)\n" title "曲")
                          ("list" "(define-metadata memo (list \"a\" \"b\"))\n" memo ("a" "b"))
                          ("quote" "(define-metadata memo '(\"a\" \"b\"))\n" memo ("a" "b"))
                          ("number" "(define-metadata title 1)\n" title 1)
                          ("key" "(define-metadata Title \"x\")\n" title "x"))]
            (let1 r (meta-load #"meta-~(car case)" (cadr case))
              (test* #"define-metadata evaluates its value: ~(car case)"
                     (list (cadddr case) '())
                     (list (value-of r (caddr case)) (caddr r)))))
          (let1 r (meta-load "meta-old" "; meta\n;\n(define-metadata memo (\"a\" \"b\"))\n")
            (test* "the old define-metadata form is accepted with a warning"
                   (list '("a" "b") (list (old-warning #"~(car r):3: " "memo")))
                   (list (value-of r 'memo) (caddr r))))
          (dolist [case `(("not-called" #f) ("called-twice" ,(^[] (%test-meta) (%test-meta))))]
            (let1 r (meta-load #"meta-old-~(car case)" "(define (%test-meta) (define-metadata memo (\"a\")))\n"
                               (cadr case))
              (test* #"the old define-metadata form warns once at expansion: ~(car case)"
                     1
                     (length (caddr r)))))
          (test* "the old define-metadata form without a location warns without it"
                 (list (old-warning "" "memo"))
                 (filter #/^warning:/
                         (string-split (call-with-output-string
                                         (^p (with-error-to-port p
                                               (^[] (parameterize ([sxmml-environment (make <sxmml-environment>)])
                                                      (eval (list 'define-metadata 'memo (list "a")) (interaction-environment)))))))
                                       "\n")))
          ;; 式が混ざった括弧は旧書式ではないので、式として評価して誤りになる
          (let1 r (meta-load "meta-mixed" "(define-metadata memo (\"a\" (string-append \"b\" \"c\")))\n")
            (test* "a mixed define-metadata value is evaluated, not taken as the old form"
                   '(#t ())
                   (list (and (condition-has-type? (cadr r) <error>) (not (sxmml-error? (cadr r)))) (caddr r))))
          ;; 空の括弧も旧書式ではない。Gauche は () を評価すると () を返すので、空のリストになる
          (let1 r (meta-load "meta-empty" "(define-metadata memo ())\n")
            (test* "an empty define-metadata value is evaluated, not taken as the old form"
                   '(() ())
                   (list (value-of r 'memo) (caddr r))))
          (dolist [case '(("no-value" "") ("too-many" " \"a\" \"b\""))]
            (let1 r (meta-load #"meta-~(car case)" #"; meta\n(begin\n  (define-metadata title~(cadr case))\n  1)\n")
              (test* #"a wrong define-metadata form fails with its location: ~(car case)"
                     (list #t #"invalid define-metadata form: expected (define-metadata key value)" #"~(car r):3" #t)
                     (let1 e (cadr r)
                       (list (sxmml-form-syntax-error? e) (condition-message e)
                             (sxmml-source-context-location e) (sxmml-form-syntax-error? e))))))
          (let1 r (meta-load "meta-undefined" "; meta\n(begin\n  (define-metadata title %test-undefined-meta)\n  1)\n")
            (test* "metadata内の未定義変数を変換しない"
                   '(#t #f #f)
                   (let1 e (cadr r)
                     (list (condition-has-type? e <unbound-variable-error>)
                             (sxmml-error? e) (sxmml-source-context? e)))))

          )
      )
    (remove-directory* workdir)))

(test-end :exit-on-failure #t)
