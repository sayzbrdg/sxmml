;;;
;;; sxmml.classesの検査
;;;

(use gauche.test)

(test-start "sxmml.classes")
(use sxmml.classes)
(test-module 'sxmml.classes)

(test* "構文・値・参照を共通のエラーとして区別できる"
       '(#t #t #t #t #t #t #f)
       (list (sxmml-error? (make-condition <sxmml-mml-syntax-error> 'message "mml"))
             (sxmml-syntax-error? (make-condition <sxmml-mml-syntax-error> 'message "mml"))
             (sxmml-syntax-error? (make-condition <sxmml-form-syntax-error> 'message "form"))
             (sxmml-form-syntax-error? (make-condition <sxmml-form-syntax-error> 'message "form"))
             (sxmml-error? (make-condition <sxmml-value-error> 'message "value"))
             (sxmml-error? (make-condition <sxmml-reference-error> 'message "reference"))
             (sxmml-value-error? (make-condition <sxmml-reference-error> 'message "reference"))))
(test* "位置とトラックの文脈はエラーとは独立したconditionである"
       '(#t #f #t #f)
       (let ([source (make-condition <sxmml-source-context> 'source-location "a.sxl:3")]
             [track (make-condition <sxmml-track-context> 'track-name 'G)])
         (list (condition? source) (sxmml-error? source)
               (condition? track) (sxmml-error? track))))
(test* "廃止したフォームエラーの公開名は残さない"
       '()
       (filter (cut memq <> (module-exports (find-module 'sxmml.classes)))
               '(<sxmml-form-error> sxmml-form-error? sxmml-form-error-location sxmml-form-error-cause)))

(test-end :exit-on-failure #t)
