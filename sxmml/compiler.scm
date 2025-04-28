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
  (export sxmml-compile-mml sxmml-compile-rhythm sxmml-compile-string sxmml-compile-mml/cc sxmml-compile-rhythm/cc
          mml rhythm
          ))
(select-module sxmml.compiler)

;;;
;;; compiler
;;;

(define (sxmml-compile-mml str ctx)
  (sxmml-compile-string (make-mml-parser ctx) str ctx))

(define (sxmml-compile-rhythm str ctx)
  (sxmml-compile-string (make-rhythm-parser ctx) str ctx))

(define (sxmml-compile-string parser str ctx)
  (unless (string? str)
    (error "string is expected, but something else is given: " track str))
  (guard (e [(is-a? e <parse-error>)
             (let ([rest (tree->string (slot-ref e 'rest))]
                   [ch (slot-ref ctx 'track)])
               (errorf "sxmml syntax error, near \"~a\" on ~a track" rest ch))])
    (values
     ;(generator->lseq (peg-parser->generator parser (generator->lseq (string->generator str))))
     (generator->list (peg-parser->generator parser (generator->lseq (string->generator str))))
     ctx)))

(define (sxmml-compile-mml/cc track str)
  (let1 ctx (sxmml-track-get track)
    (unless ctx
      (error "no defined track: " track))
    (sxmml-compile-mml str ctx)))

(define (sxmml-compile-rhythm/cc track str)
  (let1 ctx (sxmml-track-get track)
    (unless ctx
      (error "no defined track: " track))
    (sxmml-compile-rhythm str ctx)))

;;
;;  syntax
;;

(define-syntax mml
  (er-macro-transformer
    (^[form rename id=?]
      (define (build-regist track body)
        (quasirename rename
          `(sxmml-sio-regist!
            (sxmml-compile-mml/cc ',track (write-to-string (list ,@body) write-tree)) ',track)))
      (match form
        [(_ (track ..1) body ...)
         (quasirename rename
           `(begin
              ,@(fold (^[t r] (cons (build-regist t body) r)) () track)))]
        [(_ track body ...)
         (build-regist track body)]
        ))))

(define-syntax rhythm
  (er-macro-transformer
    (^[form rename id=?]
      (define (build-regist track body)
        (quasirename rename
          `(sxmml-sio-regist!
            (sxmml-compile-rhythm/cc ',track (write-to-string (list ,@body) write-tree)) ',track)))
      (match form
        [(_ (track ..1) body ...)
         (quasirename rename
           `(begin
              ,@(fold (^[t r] (cons (build-regist t body) r)) () track)))]
        [(_ track body ...)
         (build-regist track body)]
        ))))
