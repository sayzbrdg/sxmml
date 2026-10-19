;;; Copyright (c) 2024-2025 Seiji Ohashi <sayzbrdg@gmail.com>
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;  1. Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;  2. Redistributions in binary form must reproduce the above copyright
;;;     notice, this list of conditions and the following disclaimer in the
;;;     documentation and/or other materials provided with the distribution.
;;;  3. Neither the name of the authors nor the names of its contributors
;;;     may be used to endorse or promote products derived from this
;;;     software without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;; HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;; TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;; 定義・設定のマクロと加工マクロの誤りと警告を、ソースの位置付きで知らせる
(define-module sxmml.form
  (use util.match)
  (use sxmml.classes)
  (export sxmml-form-location sxmml-form-error sxmml-form-warn sxmml-form-warn-at sxmml-form-call sxmml-form-wrap))
(select-module sxmml.form)

;; フォームの開始位置を "ファイル:行" で返す。わからなければ #f
(define (sxmml-form-location form)
  (match (debug-source-info form)
    [(file line . _) #"~|file|:~|line|"]
    [_ #f]))

;; 展開時の形の誤りを、フォームの位置を付けて投げる
(define (sxmml-form-error form fmt . args)
  (let ([location (sxmml-form-location form)]
        [message (apply format #f fmt args)])
    (raise (condition (<sxmml-form-error>
                       (message (%located-message location message))
                       (location location)
                       (cause #f))))))

;; 定義のマクロの警告を、フォームの位置を付けて標準エラー出力に書く
(define (sxmml-form-warn form fmt . args)
  (apply sxmml-form-warn-at (sxmml-form-location form) fmt args))

;; 実行時の警告を、展開時に取った位置 location を付けて標準エラー出力に書く
(define (sxmml-form-warn-at location fmt . args)
  (format (current-error-port) "warning: ~a\n"
          (%located-message location (apply format #f fmt args))))

;; thunk の中で起きたエラーを、定義の位置 location を付けて投げ直す。内側の定義の
;; 誤りはその位置を残し、エラーでない値や condition はそのまま通す
(define (sxmml-form-call location thunk)
  (if location
    (guard (e [(sxmml-form-error? e) (raise e)]
              [(error? e)
               (raise (condition (<sxmml-form-error>
                                  (message (%located-message location (condition-message e)))
                                  (location location)
                                  (cause e))))])
      (thunk))
    (thunk)))

;; 定義のマクロの展開で、登録の呼び出し call を、フォームの位置を付ける
;; sxmml-form-call で包む。rename はマクロの名前の付け替えの手続きである
(define (sxmml-form-wrap form rename call)
  `(,(rename 'sxmml-form-call) ,(sxmml-form-location form) (,(rename 'lambda) () ,call)))

(define (%located-message location message)
  (if location #"~|location|: ~message" message))
