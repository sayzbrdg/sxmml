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
  (use sxmml.form)
  (export mml-repeat mml-echo mml-delay
          ))
(select-module sxmml.processor)

;;;
;;;  syntax
;;;

;; トラック名の指定として受け付けるか。空のリストは受け付けない
(define (%track-spec? x)
  (not (null? x)))

;; トラック名一つか、トラック名のリストを、トラック名のリストにする
(define (%track->track-list track)
  (if (list? track) track (list track)))

;; 本文は begin に並べ、その間に反復の開始・脱出・終了の登録を挟む。回数は式として
;; 評価し、実行時に検査する。形は展開時に検査し、誤りには、展開時でも実行時でも、
;; このフォームの位置を付ける
(define-syntax mml-repeat
  (er-macro-transformer
    (^[form rename id=?]
      ;; MML の文字列 (または文字列を返す式) cmd を、tracks の各トラックに登録する
      (define (build-registration tracks cmd)
        (map (^[t] (sxmml-form-wrap form rename
                     (quasirename rename
                       `(sxmml-ir-register! (sxmml-compile-track-mml ',t ,cmd) ',t))))
             tracks))
      ;; 脱出位置は breaks のトラックに置く。breaks が空なら置かない
      (define (expand tracks times body1 breaks body2 body3)
        `(,(rename 'begin)
          ,@(build-registration tracks "[")
          ,@body1
          ,@(build-registration breaks ":")
          ,@body2
          ,@(build-registration tracks (quasirename rename `(%mml-repeat-end-format ,times)))
          ,@body3))
      (match form
        ;; 1st format
        [(_ (? %track-spec? track) times (body ...))
         (expand (%track->track-list track) times body () () ())]
        ;; 2nd format
        [(_ (? %track-spec? track) times (body1 ...) (body2 ...))
         (expand (%track->track-list track) times body1 (%track->track-list track) body2 ())]
        ;; 3rd format
        [(_ (track ..1) times (body1 ...) (body2 ...) ((break ...) body3 ...))
         (expand track times body1 break body2 body3)]
        [(_ (track ..1) times (body1 ...) (body2 ...) (break body3 ...))
         (expand track times body1 (list break) body2 body3)]
        [(_ (? %track-spec? track) times (body1 ...) (body2 ...) (body3 ...))
         (expand (list track) times body1 (list track) body2 body3)]
        [_ (sxmml-form-error form "invalid mml-repeat form: expected (mml-repeat track count (body1 ...) [(body2 ...) [(body3 ...)]])")]))))

;; 反復の終了の MML を、回数を検査して作る
(define (%mml-repeat-end-format count)
  (%processor-value-check "mml-repeat" "count" count %positive-integer? "a positive integer")
  #"]~count")

;; 加工マクロ macro の値 name を検査する。誤りは、マクロと値の名前を示すエラーとする
(define (%processor-value-check macro name value ok? expected)
  (unless (ok? value)
    (errorf "invalid ~a value: ~a is ~s (expected ~a)" macro name value expected)))

(define (%positive-integer? x)
  (and (exact-integer? x) (positive? x)))


;;;  delay
;;;
(define (sxmml-ir-self-echo! ir fir vol rep)
  (define (build-vol val)
    (make <sxmml-mml-volume-change> :value val))
  (define (build-prev nn t)
    (make <sxmml-mml-note> :value nn :ticks t))
  (define (self-echo! obj prev)
    (let ([t (slot-ref obj 'ticks)]
          [len (ash (slot-ref obj 'ticks) -1)])
      (slot-set! obj 'ticks len)
      (list obj (build-vol vol) (build-prev prev (- t len)))))
  (define (rest-echo! obj prev)
    (let ([t (slot-ref obj 'ticks)]
          [len (ash (slot-ref obj 'ticks) -1)])
      (slot-set! obj 'ticks (- t len))
      (list (build-vol rep) (build-prev prev len) obj (build-vol (* -1 (+ vol rep))))))
  (define (make-prev)
    (let loop ([rest (sxmml-compile-mml! fir (make <sxmml-mml-context>))])
      (cond [(null? rest)
             (error "no note data: " fir)]
            [(is-a? (car rest) <sxmml-mml-rest>)
             (loop (cdr rest))]
            [(is-a? (car rest) <sxmml-mml-note>)
             (slot-ref (car rest) 'value)]
            [else
             (loop (cdr rest))])))
  (%processor-value-check "mml-echo" "voldown" vol exact-integer? "an integer")
  (%processor-value-check "mml-echo" "restdown" rep exact-integer? "an integer")
  (let loop ([rest ir]
             [prev (make-prev)]
             [vol? #f]                  ; restore volume?
             [rstack ()]                ; repeat stack
             [res ()])
    (cond [(and vol? (null? rest))
           (reverse! (cons (build-vol (* -1 vol)) res))]
          [(null? rest)
           (reverse! res)]
          [(is-a? (car rest) <sxmml-mml-rest>)
           (loop (cdr rest) prev #f rstack (cons (rest-echo! (car rest) prev) res))]
          [(and vol? (is-a? (car rest) <sxmml-mml-note>))
           (loop (cdr rest) (slot-ref (car rest) 'value) #t rstack (acons (build-vol (* -1 vol)) (self-echo! (car rest) prev) res))]
          [(is-a? (car rest) <sxmml-mml-note>)
           (loop (cdr rest) (slot-ref (car rest) 'value) #t rstack (cons (self-echo! (car rest) prev) res))]
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
         (sxmml-form-wrap form rename
           (quasirename rename
             `(sxmml-ir-register!
               (sxmml-ir-self-echo! (sxmml-compile-track-mml ',track (write-to-string (list ,@body) write-tree))
                                    ,firstnote ,voldown ,restdown)
               ',track)))]
        [_ (sxmml-form-error form "invalid mml-echo form: expected (mml-echo track (firstnote voldown restdown) body ...)")]))))


(define (sxmml-ir-self-delay! ir tick vol rep)
  (define (build-vol val)
    (make <sxmml-mml-volume-change> :value val))
  (define (build-prev nn t)
    (make <sxmml-mml-note> :value nn :ticks t))
  (define (self-delay! obj prev)
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
  (%processor-value-check "mml-delay" "ticks" tick %positive-integer? "a positive integer")
  (%processor-value-check "mml-delay" "voldown" vol exact-integer? "an integer")
  (%processor-value-check "mml-delay" "restdown" rep exact-integer? "an integer")
  (let loop ([rest ir]
             [prev #f]
             [res ()])
    (cond [(null? rest)
           (reverse! res)]
          [(is-a? (car rest) <sxmml-mml-rest>)
           (loop (cdr rest) #f (if prev (cons (self-delay! (car rest) prev) res) (cons (car rest) res)))]
          [(is-a? (car rest) <sxmml-mml-note>)
           (loop (cdr rest) (slot-ref (car rest) 'value) (cons (car rest) res))]
          [else
           (loop (cdr rest) prev (cons (car rest) res))])))

(define-syntax mml-delay
  (er-macro-transformer
    (^[form rename id=?]
      (define (build-registration track ticks voldown restdown body)
        (sxmml-form-wrap form rename
          (quasirename rename
            `(sxmml-ir-register!
              (sxmml-ir-self-delay! (sxmml-compile-track-mml ',track (write-to-string (list ,@body) write-tree))
                                    ,ticks ,voldown ,restdown)
              ',track))))
      (match form
        [(_ (? %track-spec? track) (ticks voldown restdown) body ...)
         `(,(rename 'begin)
           ,@(map (cut build-registration <> ticks voldown restdown body) (%track->track-list track)))]
        [_ (sxmml-form-error form "invalid mml-delay form: expected (mml-delay track (ticks voldown restdown) body ...)")]))))
