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

(define-module sxmml.processor
  (use util.match)
  (use text.tree)
  (use sxmml.classes)
  (use sxmml.environment)
  (use sxmml.compiler)
  (export mml-repeat mml-echo mml-delay
          ))
(select-module sxmml.processor)

;;;
;;;  syntax
;;;

(define-syntax mml-repeat
  (er-macro-transformer
    (^[form rename id=?]
      (define pi? (every-pred integer? positive?))
      (define (build-regist track obj)
        (quasirename rename
          `(sxmml-sio-regist!
            (sxmml-compile-mml/cc ',track ,(write-to-string obj write-tree)) ',track)))
      (match form
        ;; 1st format
        [(_ (track ..1) (? pi? times) (body ...))
         `(,(rename 'begin)
           ,@(fold (^[t r] (cons (build-regist t "[") r)) () track)
           ,@body
           ,@(fold (^[t r] (cons (build-regist t `("]" ,times)) r)) () track))]
        [(_ track (? pi? times) (body ...))
         `(,(rename 'begin)
           ,(build-regist track "[")
           ,@body
           ,(build-regist track `("]" ,times)))]
        ;; 2nd format
        [(_ (track ..1) (? pi? times) (body1 ...) (body2 ...))
         `(,(rename 'begin)
           ,@(fold (^[t r] (cons (build-regist t "[") r)) () track)
           ,@body1
           ,@(fold (^[t r] (cons (build-regist t ":") r)) () track)
           ,@body2
           ,@(fold (^[t r] (cons (build-regist t `("]" ,times)) r)) () track))]
        [(_ track (? pi? times) (body1 ...) (body2 ...))
         `(,(rename 'begin)
           ,(build-regist track "[")
           ,@body1
           ,(build-regist track ":")
           ,@body2
           ,(build-regist track `("]" ,times)))]
        ;; 3rd format
        [(_ (track ..1) (? pi? times) (body1 ...) (body2 ...) ((break ...) body3 ...))
         `(,(rename 'begin)
           ,@(fold (^[t r] (cons (build-regist t "[") r)) () track)
           ,@body1
           ,@(fold (^[t r] (cons (build-regist t ":") r)) () break)
           ,@body2
           ,@(fold (^[t r] (cons (build-regist t `("]" ,times)) r)) () track)
           ,@body3)]
        [(_ (track ..1) (? pi? times) (body1 ...) (body2 ...) (break body3 ...))
         `(,(rename 'begin)
           ,@(fold (^[t r] (cons (build-regist t "[") r)) () track)
           ,@body1
           ,(build-regist break ":")
           ,@body2
           ,@(fold (^[t r] (cons (build-regist t `("]" ,times)) r)) () track)
           ,@body3)]
        [(_ track (? pi? times) (body1 ...) (body2 ...) (body3 ...))
         `(,(rename 'begin)
           ,(build-regist track "[")
           ,@body1
           ,(build-regist track ":")
           ,@body2
           ,(build-regist track `("]" ,times))
           ,@body3)]
        ))))


;;;  delay
;;;
(define (sxmml-sio-self-echo sio fir vol rep)
  (define (build-vol val)
    (make <sxmml-mml-volume-change> :value val))
  (define (build-prev nn t)
    (make <sxmml-mml-note> :value nn :ticks t))
  (define (self-echo obj prev)
    (let ([t (slot-ref obj 'ticks)]
          [len (ash (slot-ref obj 'ticks) -1)])
      (slot-set! obj 'ticks len)
      (list obj (build-vol vol) (build-prev prev (- t len)))))
  (define (rest-echo obj prev)
    (let ([t (slot-ref obj 'ticks)]
          [len (ash (slot-ref obj 'ticks) -1)])
      (slot-set! obj 'ticks (- t len))
      (list (build-vol rep) (build-prev prev len) obj (build-vol (* -1 (+ vol rep))))))
  (define (make-prev)
    (let loop ([rest (sxmml-compile-mml fir (make <sxmml-mml-context>))])
      (cond [(null? rest)
             (error "no note data: " fir)]
            [(is-a? (car rest) <sxmml-mml-rest>)
             (loop (cdr rest))]
            [(is-a? (car rest) <sxmml-mml-note>)
             (slot-ref (car rest) 'value)]
            [else
             (loop (cdr rest))])))
  (let loop ([rest sio]
             [prev (make-prev)]
             [vol? #f]                  ; restore volume?
             [rstack ()]                ; repeat stack
             [res ()])
    (cond [(and vol? (null? rest))
           (reverse! (cons (build-vol (* -1 vol)) res))]
          [(null? rest)
           (reverse! res)]
          [(is-a? (car rest) <sxmml-mml-rest>)
           (loop (cdr rest) prev #f rstack (cons (rest-echo (car rest) prev) res))]
          [(and vol? (is-a? (car rest) <sxmml-mml-note>))
           (loop (cdr rest) (slot-ref (car rest) 'value) #t rstack (acons (build-vol (* -1 vol)) (self-echo (car rest) prev) res))]
          [(is-a? (car rest) <sxmml-mml-note>)
           (loop (cdr rest) (slot-ref (car rest) 'value) #t rstack (cons (self-echo (car rest) prev) res))]
          [(is-a? (car rest) <sxmml-mml-repeat-end>)
           (if (car rstack)
             (loop (cdr rest) (car rstack) #f (cdr rstack) (cons (if vol?
                                                                   (list (build-vol (* -1 vol)) (car rest))
                                                                   (car rest))
                                                                 res))
             (loop (cdr rest) prev #f (cdr rstack) (cons (if vol?
                                                           (list (build-vol (* -1 vol)) (car rest))
                                                           (car rest))
                                                         res)))]
          [(is-a? (car rest) <sxmml-mml-repeat-break>)
           (loop (cdr rest) prev #f (cons prev rstack) (cons (if vol? (list (build-vol (* -1 vol)) (car rest)) (car rest)) res))]
          [(is-a? (car rest) <sxmml-mml-repeat-start>)
           (loop (cdr rest) prev vol? (cons #f rstack) (cons (car rest) res))]
          [else
           (loop (cdr rest) prev vol? rstack (cons (car rest) res))])))

(define-syntax mml-echo
  (er-macro-transformer
    (^[form rename id=?]
      (match form
        [(_ track (firstnote voldown restdown) body ...)
         (quasirename rename
           `(sxmml-sio-regist!
             (sxmml-sio-self-echo (sxmml-compile-mml/cc ',track (write-to-string (list ,@body) write-tree))
                                  ,firstnote ,voldown ,restdown)
             ',track))]))))


(define (sxmml-sio-self-delay sio tick vol rep)
  (define (build-vol val)
    (make <sxmml-mml-volume-change> :value val))
  (define (build-prev nn t)
    (make <sxmml-mml-note> :value nn :ticks t))
  (define (self-delay obj prev)
    (let1 orig-tick (slot-ref obj 'ticks)
      (if (>= tick orig-tick)
        obj
        (begin
          (slot-set! obj 'ticks tick)
          (let loop ([t (- orig-tick tick)]
                     [cnt vol]
                     [res (list (build-vol vol) obj)])
            (if (> t 0)
              (loop (- t tick) (+ cnt rep) (cons* (build-vol rep) (build-prev prev (if (> t tick) tick t)) res))
              (reverse! (cons (build-vol (* -1 cnt)) res))))))))
  (let loop ([rest sio]
             [prev #f]
             [res ()])
    (cond [(null? rest)
           (reverse! res)]
          [(is-a? (car rest) <sxmml-mml-rest>)
           (loop (cdr rest) #f (if prev (cons (self-delay (car rest) prev) res) (cons (car rest) res)))]
          [(is-a? (car rest) <sxmml-mml-note>)
           (loop (cdr rest) (slot-ref (car rest) 'value) (cons (car rest) res))]
          [else
           (loop (cdr rest) prev (cons (car rest) res))])))

(define-syntax mml-delay
  (er-macro-transformer
    (^[form rename id=?]
      (define pi? (every-pred integer? positive?))
      (define (build-regist track body ticks voldown restdown)
        (quasirename rename
           `(sxmml-sio-regist!
             (sxmml-sio-self-delay (sxmml-compile-mml/cc ',track (write-to-string (list ,@body) write-tree))
                                   ,ticks ,voldown ,restdown)
             ',track)))
      (match form
        [(_ (track ..1) ((? pi? ticks) (? integer? voldown) (? integer? restdown)) body ...)
         (quasirename rename
           `(begin
              ,@(fold (^[t r] (cons (build-regist t body ticks voldown restdown) r)) () track)))]
        [(_ track ((? pi? ticks) (? integer? voldown) (? integer? restdown)) body ...)
         (build-regist track body ticks voldown restdown)]
        ))))
