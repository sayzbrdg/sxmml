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
           (logior (ash dt2 5) sr)
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
     (errorf "invalid voice format: @~a, operator(~a), ~a" id openum form)]))

;; 音色を検査して登録する。演算子の値は 12 値に直し、どの値も 0 以上の正確な整数とする。
;; 同じ id の音色は置き換える。番号はバックエンドが決めるので、ここでは持たない
(define (sxmml-opn-voice-set! id alfb op1 op2 op3 op4)
  (unless (symbol? id)
    (errorf "voice id must be a symbol: ~a" id))
  (unless (and (list? alfb) (= (length alfb) 2))
    (errorf "invalid voice format: @~a, al/fb, ~a" id alfb))
  (let1 ops (map (cut %4op-voice-shape <> id <>) (list op1 op2 op3 op4) '(1 2 3 4))
    (%voice-values-check alfb id "al/fb")
    (for-each (^[op openum] (%voice-values-check op id #"operator(~openum)")) ops '(1 2 3 4))
    (hash-table-set! (slot-ref (sxmml-environment) 'voice) id
                     (make <sxmml-opn-voice> :id id :value (cons alfb ops)))))

(define (%voice-values-check values id place)
  (if-let1 v (find (^v (not (and (exact-integer? v) (>= v 0)))) values)
    (errorf "invalid voice value: @~a, ~a, ~a" id place v)))

;; 登録は実行時に行うので、展開時には音色表を見ない
(define-syntax define-opn-voice
  (er-macro-transformer
    (^[form rename id=?]
      (match form
        [(_ id (al fb) op1 op2 op3 op4)
         (let1 idsym (string->symbol (x->string id))
           (quasirename rename
             `(sxmml-opn-voice-set! ',idsym '(,al ,fb) ',op1 ',op2 ',op3 ',op4)))]))))
