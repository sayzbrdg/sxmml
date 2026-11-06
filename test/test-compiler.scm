;;;
;;; sxmml.compilerの検査
;;;

(use gauche.test)

(test-start "sxmml.compiler")
(use sxmml.compiler)
(test-module 'sxmml.compiler)

(use parser.peg)
(use sxmml.classes)
(use sxmml.environment)
(use sxmml)
(use file.util)

(define (failure-result parser)
  (guard (e [else (list (sxmml-mml-syntax-error? e) (condition-message e)
                        (sxmml-track-context-track e) (condition-has-type? e <parse-error>))])
    (sxmml-compile-string! parser "x" (make <sxmml-mml-context> :track 'G))))
(test* "明示的なPEG失敗をトラック付きのMML構文エラーに変換する"
       '(#t "explicit parser failure" G #f)
       (failure-result ($raise "explicit parser failure")))
(test* "通常のPEG失敗も元のconditionを持ち回らず変換する"
       '(#t "invalid MML syntax near \"x\"" G #f)
       (failure-result ($. #\a)))
(let1 original (make-condition <error> 'message "parser bug")
  (test* "非PEGの一般エラーは変換しない" #t
         (guard (e [else (eq? e original)])
           (sxmml-compile-string! (^[_] (raise original)) "x"
                                 (make <sxmml-mml-context> :track 'G)))))
(test* "未定義トラックは参照エラーになる"
       '(#t X)
       (parameterize ([sxmml-environment (make <sxmml-environment>)])
         (guard (e [else (list (sxmml-reference-error? e) (sxmml-track-context-track e))])
           (sxmml-compile-track-mml 'X "c"))))
(test* "非文字列引数はトラック付きの値エラーになる"
       '(#t G)
       (guard (e [else (list (sxmml-value-error? e) (sxmml-track-context-track e))])
         (sxmml-compile-string! ($. #\a) 123 (make <sxmml-mml-context> :track 'G))))
(test* "G01 通常MMLの空IRと解析の継続"
       '(() ((<sxmml-mml-note> 60 12) (<sxmml-mml-note> 62 6)) ((18 . 0)) 16)
       (let* ([ctx (make <sxmml-mml-context>)]
              [empty (sxmml-compile-mml! "l8o5" ctx)]
              [ir (sxmml-compile-mml! "cl16d" ctx)])
         (list empty
               (map (^[o] (list (class-name (class-of o)) (slot-ref o 'value) (slot-ref o 'ticks))) ir)
               (slot-ref ctx 'current-ticks) (slot-ref ctx 'default-length))))
(test* "G02 リズムMMLの解析の継続"
       '((<sxmml-rhythm-emit> <sxmml-mml-rest>) #t 12 #f 6 ((18 . 0)) 16)
       (let* ([ctx (make <sxmml-mml-context>)] [ir (sxmml-compile-rhythm! "l8@bl16r" ctx)])
         (list (map (compose class-name class-of) ir)
               (slot-ref (car ir) 'bass) (slot-ref (car ir) 'ticks)
               (slot-ref (cadr ir) 'value) (slot-ref (cadr ir) 'ticks)
               (slot-ref ctx 'current-ticks) (slot-ref ctx 'default-length))))
(test* "C01 本文を一度評価し各トラックの音高と音長で解釈する"
       '(1 ((48 12)) ((60 24)) #f ((12 . 0)) ((24 . 0)))
       (parameterize ([sxmml-environment (make <sxmml-environment>)])
         (sxmml-track-set! 'A :default-length 8 :current-octave 4)
         (sxmml-track-set! 'B :default-length 4 :current-octave 5)
         (let1 calls 0
           (mml (A B) (begin (inc! calls) "c"))
           (let* ([a (sxmml-track-get 'A)] [b (sxmml-track-get 'B)]
                  [ai (slot-ref a 'compiled-data)] [bi (slot-ref b 'compiled-data)])
             (list calls
                   (map (^o (list (slot-ref o 'value) (slot-ref o 'ticks))) ai)
                   (map (^o (list (slot-ref o 'value) (slot-ref o 'ticks))) bi)
                   (eq? (car ai) (car bi))
                   (slot-ref a 'current-ticks) (slot-ref b 'current-ticks))))))
(test* "C02 リズム本文を一度評価し各トラックの音長で解釈する"
       '(1 #t 12 #t 24 ((12 . 0)) ((24 . 0)))
       (parameterize ([sxmml-environment (make <sxmml-environment>)])
         (sxmml-track-set! 'A :default-length 8)
         (sxmml-track-set! 'B :default-length 4)
         (let1 calls 0
           (rhythm (A B) (begin (inc! calls) "@b"))
           (let* ([a (sxmml-track-get 'A)] [b (sxmml-track-get 'B)]
                  [ai (car (slot-ref a 'compiled-data))] [bi (car (slot-ref b 'compiled-data))])
             (list calls (slot-ref ai 'bass) (slot-ref ai 'ticks)
                   (slot-ref bi 'bass) (slot-ref bi 'ticks)
                   (slot-ref a 'current-ticks) (slot-ref b 'current-ticks))))))
(test* "C03 本文なしでも文脈を維持する" '(() ((0 . 0)) 4 4)
       (parameterize ([sxmml-environment (make <sxmml-environment>)])
         (sxmml-track-set! 'A)
         (mml A)
         (let1 ctx (sxmml-track-get 'A)
           (map (cut slot-ref ctx <>) '(compiled-data current-ticks default-length current-octave)))))

(let1 workdir (sys-mkdtemp (build-path (temporary-directory) "sxmml-compiler"))
  (define (load-form name form)
    (let1 source (build-path workdir #"~|name|.sxl")
      (with-output-to-file source (^[] (format #t "; compiler\n(begin\n ~a\n 1)\n" form)))
      (parameterize ([sxmml-environment (make <sxmml-environment>)])
        (sxmml-track-set! 'A)
        (sxmml-track-set! 'B)
        (let1 e (guard (e [else e]) (load source) #f)
          (list source e (sxmml-environment))))))
  (define (location result)
    (and (sxmml-source-context? (cadr result)) (sxmml-source-context-location (cadr result))))
  (define (objects result track)
    (slot-ref (hash-table-get (slot-ref (caddr result) 'track) track) 'compiled-data))
  (unwind-protect
      (begin
        (let1 r (load-form "partial" "(mml (A X) \"c8\")")
          (test* "C04 指定順で先行登録を終えてから未登録トラックで失敗する"
                 (list #t "undefined track: X" #"~(car r):3" 'X '((48 12)))
                 (list (sxmml-reference-error? (cadr r)) (condition-message (cadr r)) (location r)
                       (sxmml-track-context-track (cadr r))
                       (map (^o (list (slot-ref o 'value) (slot-ref o 'ticks))) (objects r 'A)))))
        (test* "C05 利用者マクロの名前と本文を保ち生成識別子を捕捉させない"
               '(#f A ((48 12)))
               (let1 r (load-form "hygiene"
                          "(define-syntax generated-mml (syntax-rules () [(_ name phrase) (mml name phrase)]))\n(let ((text \"c8\") (write-to-string #f) (list #f)\n      (sxmml-compile-track-mml #f) (sxmml-ir-register! #f))\n  (generated-mml A text))")
                 (list (cadr r)
                       (slot-ref (hash-table-get (slot-ref (caddr r) 'track) 'A) 'track)
                       (map (^o (list (slot-ref o 'value) (slot-ref o 'ticks))) (objects r 'A)))))
        (let1 r (load-form "bad-form" "(mml)")
          (test* "C06 不正な書式をフォーム位置付きで拒否する"
                 (list #t "invalid mml form: expected (mml track body ...) or (mml (track ...) body ...)" #"~(car r):3")
                 (list (sxmml-form-syntax-error? (cadr r)) (condition-message (cadr r)) (location r))))
        (let1 r (load-form "no-tracks" "(rhythm () (error \"評価しない\"))")
          (test* "C07 空のトラック指定では本文を評価しない"
                 (list #t "rhythm requires at least one track name" #"~(car r):3")
                 (list (sxmml-form-syntax-error? (cadr r)) (condition-message (cadr r)) (location r))))
        (let1 r (load-form "bad-name" "(mml (A 0) \"c8\")")
          (test* "C08 不正な名前は対象値と位置を示し登録前に拒否する"
                 (list #t "invalid mml track name: 0 (expected a symbol)" #"~(car r):3" '())
                 (list (sxmml-form-syntax-error? (cadr r)) (condition-message (cadr r))
                       (location r) (objects r 'A))))
        (let1 r (load-form "known-body-error"
                          "(mml (A B) (error <sxmml-value-error> \"sample value error\"))")
          (test* "C09 共有本文の既知エラーへ位置を付けトラックを補わない"
                 (list #t "sample value error" #"~(car r):3" #f)
                 (list (sxmml-value-error? (cadr r)) (condition-message (cadr r))
                       (location r) (sxmml-track-context? (cadr r)))))
        (let1 r (load-form "unknown-body-error" "(mml A (error \"sample Scheme error\"))")
          (test* "C10 本文の未知のGaucheエラーを包み直さない"
                 '(#t "sample Scheme error" #f #f)
                 (let1 e (cadr r)
                   (list (condition-has-type? e <error>) (condition-message e)
                         (sxmml-error? e) (sxmml-source-context? e))))))
    (remove-directory* workdir)))
(test-end :exit-on-failure #t)
