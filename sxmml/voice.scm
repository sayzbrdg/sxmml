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
  (export define-opn-voice
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

(define-syntax define-opn-voice
  (er-macro-transformer
    (^[form rename id=?]
      (match form
        [(_ id (al fb) op1 op2 op3 op4)
         (let ([idsym (string->symbol (x->string id))]
               [vdata (list (list al fb)
                            (%4op-voice-shape op1 id 1)
                            (%4op-voice-shape op2 id 2)
                            (%4op-voice-shape op3 id 3)
                            (%4op-voice-shape op4 id 4))]
               [number (hash-table-size (slot-ref (sxmml-environment) 'voice))])
           (if-let1 old (hash-table-get (slot-ref (sxmml-environment) 'voice) idsym #f)
             (quasirename rename
               `(slot-set! ,old 'value ',vdata))
             (quasirename rename
               `(hash-table-set! (slot-ref (sxmml-environment) 'voice)
                                 ',idsym (make <sxmml-opn-voice>
                                           :id ',idsym
                                           :number ',number
                                           :value ',vdata)))))]))))
