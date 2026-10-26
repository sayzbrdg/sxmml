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

(define-module sxmml.compiler
  (use util.match)
  (use parser.peg)
  (use gauche.generator)
  (use text.tree)
  (use sxmml.classes)
  (use sxmml.environment)
  (use sxmml.mml)
  (use sxmml.form)
  (export sxmml-compile-mml! sxmml-compile-rhythm! sxmml-compile-string! sxmml-compile-track-mml sxmml-compile-track-rhythm
          mml rhythm
          ))
(select-module sxmml.compiler)

;;;
;;; compiler
;;;

(define (sxmml-compile-mml! str ctx)
  (sxmml-compile-string! (make-sxmml-mml-parser ctx) str ctx))

(define (sxmml-compile-rhythm! str ctx)
  (sxmml-compile-string! (make-sxmml-rhythm-parser ctx) str ctx))

(define (sxmml-compile-string! parser str ctx)
  (let* ([ch (slot-ref ctx 'track)]
         [context (and ch (make-condition <sxmml-track-context> 'track-name ch))])
    (guard (e [(condition-has-type? e <parse-error>)
               (let1 message
                   (match (list (slot-ref e 'type) (slot-ref e 'objects))
                     [('fail-error (_ ('fail-message . (? string? reason))))
                      (format #f "sxmml syntax error: ~a on ~a track" reason ch)]
                     [_ (format #f "sxmml syntax error, near \"~a\" on ~a track"
                                (tree->string (slot-ref e 'rest)) ch)])
                 (raise (sxmml-error-with-context
                         (make-condition <sxmml-mml-syntax-error> 'message message) context)))]
              [(sxmml-error? e)
               (raise (sxmml-error-with-context e context))])
      (unless (string? str)
        (error <sxmml-value-error> "string is expected, but something else is given: " ch str))
      (values
       (generator->list (peg-parser->generator parser (generator->lseq (string->generator str))))
       ctx))))

(define (sxmml-compile-track-mml track str)
  (let1 ctx (sxmml-track-get track)
    (unless ctx
      (raise (sxmml-error-with-context
              (make-condition <sxmml-reference-error> 'message (format #f "no defined track:  ~a" track))
              (make-condition <sxmml-track-context> 'track-name track))))
    (sxmml-compile-mml! str ctx)))

(define (sxmml-compile-track-rhythm track str)
  (let1 ctx (sxmml-track-get track)
    (unless ctx
      (raise (sxmml-error-with-context
              (make-condition <sxmml-reference-error> 'message (format #f "no defined track:  ~a" track))
              (make-condition <sxmml-track-context> 'track-name track))))
    (sxmml-compile-rhythm! str ctx)))

;;
;;  syntax
;;

(define-syntax mml
  (er-macro-transformer
    (^[form rename id=?]
      (define (build-registration track body)
        (quasirename rename
          `(sxmml-ir-register!
            (sxmml-compile-track-mml ',track (write-to-string (list ,@body) write-tree)) ',track)))
      (match form
        [(_ (track ..1) body ...)
         (quasirename rename
           `(begin
              ,@(fold (^[t r] (cons (build-registration t body) r)) () track)))]
        [(_ track body ...)
         (build-registration track body)]
        ))))

(define-syntax rhythm
  (er-macro-transformer
    (^[form rename id=?]
      (define (build-registration track body)
        (quasirename rename
          `(sxmml-ir-register!
            (sxmml-compile-track-rhythm ',track (write-to-string (list ,@body) write-tree)) ',track)))
      (match form
        [(_ (track ..1) body ...)
         (quasirename rename
           `(begin
              ,@(fold (^[t r] (cons (build-registration t body) r)) () track)))]
        [(_ track body ...)
         (build-registration track body)]
        ))))
