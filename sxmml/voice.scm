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

(define-module sxmml.voice
  (use util.match)
  (use sxmml.classes)
  (use sxmml.environment)
  (use sxmml.form)
  (export define-opn-voice sxmml-opn-voice-set!
          sxmml-opmn-operator-zip))
(select-module sxmml.voice)

;;
;;  FM voice
;;

(define (sxmml-opmn-operator-zip op1 op2 op3 op4 afb)
  (reverse!
   (cons afb
         (fold (^(v1 v2 v3 v4 r)
                 (cons* v4 v3 v2 v1 r))
               () op1 op3 op2 op4))))

(define (sxmml-opm-list->register form)
  (match form
    [(ar dr sr rr sl tl ks ml dt dt2 seg ams)
     (list (logior (ash dt 4) ml)
           tl
           (logior (ash ks 6) ar)
           (logior (ash ams 7) dr)
           (logior (ash dt2 6) sr)
           (logior (ash sl 4) rr))]))

(define (%4op-voice-shape form id openum)
  (match form
    [(ar dr sr rr sl tl ks ml dt dt2 seg ams)
     form]
    [(ar dr sr rr sl tl ks ml dt seg ams)
     (%4op-voice-shape (list ar dr sr rr sl tl ks ml dt 0 seg ams) id openum)]
    [(ar dr sr rr sl tl ks ml dt ams)
     (%4op-voice-shape (list ar dr sr rr sl tl ks ml dt 0 0 ams) id openum)]
    [(ar dr sr rr sl tl ks ml dt)
     (%4op-voice-shape (list ar dr sr rr sl tl ks ml dt 0 0 0) id openum)]
    [else
     (errorf <sxmml-value-error> "invalid voice @~a operator(~a): ~s (expected a list of 9 to 12 values)" id openum form)]))

(define %voice-alfb-ranges '((AL 0 7) (FB 0 7)))
(define %voice-operator-ranges
  '((AR 0 31) (DR 0 31) (SR 0 31) (RR 0 15) (SL 0 15) (TL 0 127)
    (KS 0 3) (ML 0 15) (DT -3 7) (DT2 0 3) (SEG 0 15) (AMS 0 1)))

;; 音色を検査して登録する。演算子は12値に補完し、範囲検査後に負のDTを正規化する。
;; 同じ id の音色は置き換える。番号はバックエンドが決めるので、ここでは持たない
(define (sxmml-opn-voice-set! id alfb op1 op2 op3 op4)
  (unless (symbol? id)
    (errorf <sxmml-value-error> "invalid voice id: ~s (expected a symbol)" id))
  (unless (%voice-alfb-form? alfb)
    (errorf <sxmml-value-error> "invalid voice @~a al/fb: ~s (expected a list of 2 values)" id alfb))
  (for-each (^[op openum]
              (unless (%voice-operator-form? op)
                (errorf <sxmml-value-error> "invalid voice @~a operator(~a): ~s (expected a list of 9 to 12 values)" id openum op)))
            (list op1 op2 op3 op4) '(1 2 3 4))
  (let* ([ops (map (cut %4op-voice-shape <> id <>) (list op1 op2 op3 op4) '(1 2 3 4))]
         [checked-alfb (%voice-values-check alfb %voice-alfb-ranges id #f)]
         [checked-ops (map (^[op openum]
                             (%voice-values-check op %voice-operator-ranges id #"operator(~openum)"))
                           ops '(1 2 3 4))])
    (hash-table-set! (slot-ref (sxmml-environment) 'voice) id
                    (make <sxmml-opn-voice> :id id :value (cons checked-alfb checked-ops)))))

(define (%voice-values-check values ranges id place)
  (map (^[v range]
         (match range
           [(name low high)
            (unless (and (exact-integer? v) (<= low v high))
              (errorf <sxmml-value-error>
                      "invalid voice @~a~a ~a: ~s (expected an integer from ~a to ~a)"
                      id (if place (format #f " ~a" place) "") name v low high))
            (if (and (eq? name 'DT) (negative? v)) (+ 4 (abs v)) v)]))
       values ranges))

;; 音色の枠の要素の数の規則。マクロの展開時と、登録の手続きの両方で使う
(define (%voice-alfb-form? x)
  (and (list? x) (= (length x) 2)))
(define (%voice-operator-form? x)
  (and (list? x) (<= 9 (length x) 12)))

;; 枠の中の要素は式として評価する。形は展開時に検査し、登録は実行時に行う。
;; 定義の誤りには、展開時でも実行時でも、このフォームの位置を付ける
(define-syntax define-opn-voice
  (er-macro-transformer
    (^[form rename id=?]
      (define (form-error)
        (sxmml-form-error form "invalid define-opn-voice form: expected (define-opn-voice id (al fb) op1 op2 op3 op4)"))
      (match form
        [(_ id alfb . ops)
         (unless (and (list? ops) (= (length ops) 4) (every list? (cons alfb ops)))
           (form-error))
         (unless (%voice-alfb-form? alfb)
           (sxmml-form-error form "invalid voice @~a al/fb: ~s (expected a list of 2 values)" (unwrap-syntax id) alfb))
         (for-each (^[op openum]
                     (unless (%voice-operator-form? op)
                       (sxmml-form-error form "invalid voice @~a operator(~a): ~s (expected a list of 9 to 12 values)" (unwrap-syntax id) openum op)))
                   ops '(1 2 3 4))
         (let1 idsym (sxmml-form-name->symbol form "define-opn-voice" id)
           (sxmml-form-wrap form rename
             (quasirename rename
               `(sxmml-opn-voice-set! ',idsym (list ,@alfb)
                                      ,@(map (^[op] (cons (rename 'list) op)) ops)))))]
        [_ (form-error)]))))
