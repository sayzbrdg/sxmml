;;;
;;; sxmml.formの検査
;;;

(use gauche.test)

(test-start "sxmml.form")
(use sxmml.form)
(test-module 'sxmml.form)
(use sxmml)
(use sxmml.backend.pmd)

;; 定義の誤りの位置 (#28)
;; sxmml-form-call は、中で起きたエラーに定義の既知のエラーに位置を独立して付ける
(let ([caught (^[thunk] (guard (e [else e]) (thunk)))]
      [boom (^[] (errorf <sxmml-value-error> "boom: ~a" 1))])
  (test* "sxmml-form-call adds the location to an error"
         '(#t "boom: 1" "./a.sxl:3" #t)
         (let1 e (caught (^[] (sxmml-form-call "./a.sxl:3" boom)))
           (list (sxmml-error? e) (condition-message e) (sxmml-source-context-location e)
                 (sxmml-value-error? e))))
  (test* "sxmml-form-call without a location passes the error"
         '(#t "boom: 1")
         (let1 e (caught (^[] (sxmml-form-call #f boom)))
           (list (sxmml-error? e) (condition-message e))))
  ;; 入れ子では、いちばん内側の定義の位置だけを残す
  (test* "sxmml-form-call passes an inner form error"
         '("boom: 1" "./b.sxl:7")
         (let1 e (caught (^[] (sxmml-form-call "./a.sxl:3"
                                (^[] (sxmml-form-call "./b.sxl:7" boom)))))
           (list (condition-message e) (sxmml-source-context-location e))))
  (test* "sxmml-form-call passes a raised non-condition"
         42
         (caught (^[] (sxmml-form-call "./a.sxl:3" (^[] (raise 42))))))
  ;; 位置を付けるのはエラーだけで、続けられるエラーでない condition は、そのまま通して続けさせる
  (test* "sxmml-form-call passes a continuable non-error condition"
         '(handled 10)
         (with-exception-handler
          (^[c] (if (and (message-condition? c) (not (error? c))) 'handled (raise c)))
          (^[] (sxmml-form-call "./a.sxl:3"
                 (^[] (list (raise-continuable (make-condition <message-condition> 'message "note"))
                            10))))))
  (test* "sxmml-form-call returns the value of the thunk"
         5
         (sxmml-form-call "./a.sxl:3" (^[] 5)))
  (test* "sxmml-form-location of a constructed form is #f"
         #f
         (sxmml-form-location (list 'a 'b)))
  ;; 定義のマクロは、登録の呼び出しを sxmml-form-wrap で包んで展開する
  (test* "sxmml-form-wrap wraps a call with sxmml-form-call"
         '(sxmml-form-call #f (lambda () (foo 1)))
         (sxmml-form-wrap (list 'define-x 'a) (^[x] x) '(foo 1)))
  (test* "sxmml-form-warn without a location"
         "warning: old: 1\n"
         (call-with-output-string
           (^[p] (with-error-to-port p (^[] (sxmml-form-warn (list 'a) "old: ~a" 1))))))
  ;; 実行時の警告は、展開時に取った位置を受ける (#51)
  (test* "sxmml-form-warn-at writes a warning with the location"
         "warning: ./a.sxl:3: ignored: 1\n"
         (call-with-output-string
           (^[p] (with-error-to-port p (^[] (sxmml-form-warn-at "./a.sxl:3" "ignored: ~a" 1))))))
  (test* "sxmml-form-warn-at without a location"
         "warning: ignored: 1\n"
         (call-with-output-string
           (^[p] (with-error-to-port p (^[] (sxmml-form-warn-at #f "ignored: ~a" 1))))))
  (test* "sxmml-form-error without a location"
         '(#t "bad: 1" #f #t)
         (let1 e (caught (^[] (sxmml-form-error (list 'a) "bad: ~a" 1)))
           (list (sxmml-error? e) (condition-message e)
                 (sxmml-source-context? e) (sxmml-form-syntax-error? e)))))

(let1 original (make-condition <error> 'message "unknown")
  (test* "未知のGaucheエラーは同じオブジェクトで通過する" #t
         (guard (e [else (eq? e original)])
           (sxmml-form-call "a.sxl:3" (^[] (raise original))))))
(test* "トラックとフォーム位置を合成して型と本文を保つ"
       '(#t "value" G "a.sxl:3")
       (let* ([original (make-condition <sxmml-value-error> 'message "value")]
              [tracked (sxmml-error-with-context original
                         (make-condition <sxmml-track-context> 'track-name 'G))]
              [located (sxmml-error-with-context tracked
                         (make-condition <sxmml-source-context> 'source-location "a.sxl:3"))])
         (list (sxmml-value-error? located) (condition-message located)
               (sxmml-track-context-track located) (sxmml-source-context-location located))))
(test* "フォーム位置をGaucheの補足情報として報告する"
       "    at a.sxl:3\n"
       (call-with-output-string
         (^[p] (report-additional-condition
              (make-condition <sxmml-source-context> 'source-location "a.sxl:3") p))))

(test-end :exit-on-failure #t)
