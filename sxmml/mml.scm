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

(define-module sxmml.mml
  (use util.match)
  (use parser.peg)
  (use text.tree)
  (use sxmml.classes)
  (use sxmml.environment)
  (export make-sxmml-mml-parser make-sxmml-rhythm-parser ;$note-commands
          ))
(select-module sxmml.mml)

(define sxmml-mml-context (make-parameter (make <sxmml-mml-context>)))
(define *note-command* (make-hash-table eqv-comparator))
(define *rhythm-command* (make-hash-table eqv-comparator))

;;;
;;; mml parser
;;;
(define-inline (rope->number rope)
  (string->number (rope->string rope)))
(define-inline (rope->positive-number rope :optional (message "ticks must be a positive integer: "))
  (let1 num (rope->number rope)
    (unless (and (exact-integer? num) (positive? num))
      (error message num))
    num))
(define-inline (rope->symbol rope)
  (string->symbol (rope->string rope)))

(define-inline (change-sign number)
  (* number -1))

(define-syntax %$return
  (er-macro-transformer
    (^[form rename id=?]
      (quasirename rename
        `(lambda [seq] (values #f ,@(cdr form) seq))))))

(define (%make-mml-value-parser param class proc)
  ($let ([data param]
         [$space])
        ($return (make class :value (proc data)))))
(define (%make-mml-command-parser class)
  (%$return
   (make class)))
(define (%make-mml-slots-parser param class proc slots)
  ($let ([data param]
         [$space])
        (let ([vals (fold (^[s d r]
                            (cons* s (proc d) r))
                          ()
                          slots
                          data)])
          ($return (apply make class vals)))))
(define (%make-mml-context-value-parser param slot proc)
  ($let ([data param]
         [$space])
        (slot-set! (sxmml-mml-context) slot (proc data))
        ($return (make <sxmml-mml-nop>))))
(define (%make-mml-context-command-parser slot proc)
  (%$return
   (begin (slot-set! (sxmml-mml-context) slot (proc))
          (make <sxmml-mml-nop>))))

(define (make-sxmml-mml-parser env)
  ($parameterize ([sxmml-mml-context env]) $note-commands))

(define (make-sxmml-rhythm-parser env)
  ($parameterize ([sxmml-mml-context env]) $rhythm-commands))

;; general value
(define-inline $space ($many_ ($. #[\s])))
(define-inline $number ($many1 ($. #[0-9])))
(define-inline $integer ($list* ($optional ($. #[-+])) ($many1 ($. #[0-9]))))
(define-inline $parameter ($or $number
                               ($between ($seq ($. #\{) $space) ($many1 ($. #[^\s{}])) ($seq $space ($. #\})))))
(define-inline $number-parameter ($or $number
                              ($between ($seq ($. #\{) $space) $number ($seq $space ($. #\})))))
(define-inline $integer-parameter ($or $integer
                              ($between ($seq ($. #\{) $space) $integer ($seq $space ($. #\})))))
(define-inline $multiple-parameters ($between ($seq ($. #\{)) ($many1 ($seq $space ($many1 ($. #[^\s{}])))) ($seq $space ($. #\}))))


;; note & rest
(define-inline (%proc-note-number name accidentals)
  (let ([different (fold (^[v r]
                           (if (char=? v #\+) (+ r 1) (- r 1)))
                         0
                         accidentals)]
        [octave (slot-ref (sxmml-mml-context) 'current-octave)]
        [transpose (slot-ref (sxmml-mml-context) 'current-transpose)]
        [note (case name
                ([#\c] 0)
                ([#\d] 2)
                ([#\e] 4)
                ([#\f] 5)
                ([#\g] 7)
                ([#\a] 9)
                ([#\b] 11))])
    (+ (* octave 12) note different transpose)))

(define $note-name
  ($let ([name ($. #[a-gr])]
         [accidentals ($many ($. #[-+]))]
         [$space])
        ($return (if (char=? name #\r)
                   (make <sxmml-mml-rest> :value #f)
                   (make <sxmml-mml-note> :value (%proc-note-number name accidentals))))))

(define (%make-note-length-parser $number $dots)
  ($let ([number $number]
         [$space]
         [dots $dots])
        ($return (length->ticks (rope->number number) (length dots)))))
(define (length->ticks len dots)
  (define (dots-calculate base dots)
    (let loop ([v base] [e base] [n dots])
      (cond [(zero? n)
             v]
            [(not (logbit? 0 e))
             (let1 h (ash e -1)
               (loop (+ v h) h (- n 1)))]
            [else
             (error "number of dots exceeded resolution range")])))
  (let ([res (sxmml-resolution-get)]
        [clen (or len (slot-ref (sxmml-mml-context) 'default-length))])
    (dots-calculate (/ (ash res 2) clen) dots)))

(define-inline $note-length ($or (%make-note-length-parser $number ($many ($. #\.)))
                                (%make-note-length-parser ($return #f) ($many1 ($. #\.)))))
(define-inline $optional-note-length (%make-note-length-parser ($optional $number) ($many ($. #\.))))
(define $tick-length ($let ([($. #\%)] [$space] [num $number] [$space]) ($return (rope->positive-number num))))
(define $duration ($let ([f ($or $tick-length $optional-note-length)]
                         [l ($many ($try ($seq ($. #\&) $space ($or $tick-length $note-length))))])
                        ($return (apply + f l))))
(define $note
  ($let ([o $note-name]
         [t $duration]
         [$space])
        (let1 ct (car (slot-ref (sxmml-mml-context) 'current-ticks))
          (set! (car ct) (+ t (car ct)))
          (slot-set! o 'ticks t)
          (hash-table-update! (slot-ref (sxmml-mml-context) 'ticks-statistics) t (cut + 1 <>) 0)
          ($return o))))

;; general command
(define-inline $length (%make-mml-context-value-parser $number-parameter 'default-length rope->number))
(hash-table-set! *note-command* #\l $length)
(define-inline $octave (%make-mml-context-value-parser  $number-parameter 'current-octave rope->number))
(hash-table-set! *note-command* #\o $octave)
(define-inline $octave-up
  (%make-mml-context-command-parser 'current-octave
                       (^[] (+ (slot-ref (sxmml-mml-context) 'current-octave) 1))))
(hash-table-set! *note-command* #\> $octave-up)
(define-inline $octave-down
  (%make-mml-context-command-parser 'current-octave
                       (^[] (- (slot-ref (sxmml-mml-context) 'current-octave) 1))))
(hash-table-set! *note-command* #\< $octave-down)
(define-inline $volume (%make-mml-value-parser $number-parameter <sxmml-mml-volume> rope->number))
(define-inline $volume-change (%make-mml-value-parser $integer-parameter <sxmml-mml-volume-change> rope->number))
(define-inline $volume-command ($or $volume
                                    ($seq ($. #\*) $volume-change)))
(hash-table-set! *note-command* #\v $volume-command)
(define $volume-up
  (%$return
   (make <sxmml-mml-volume-change>
     :value (slot-ref (sxmml-mml-context) 'volume-delta))))
(hash-table-set! *note-command* #\) $volume-up)
(define $volume-down
  (%$return
   (make <sxmml-mml-volume-change>
     :value (change-sign (slot-ref (sxmml-mml-context) 'volume-delta)))))
(hash-table-set! *note-command* #\( $volume-down)
(define-inline $pan (%make-mml-value-parser  $number-parameter <sxmml-mml-pan> rope->number))
(hash-table-set! *note-command* #\p $pan)
(define-inline $gate-time (%make-mml-value-parser $number-parameter <sxmml-mml-gate-time> rope->number))
(hash-table-set! *note-command* #\q $gate-time)
(define-inline $tie (%make-mml-command-parser <sxmml-mml-tie>))
(hash-table-set! *note-command* #\& $tie)
(define-inline $transpose (%make-mml-value-parser $integer-parameter <sxmml-mml-transpose> rope->number))
(define-inline $transpose-env (%make-mml-context-value-parser $integer-parameter 'current-transpose rope->number))
(define-inline $transpose-change (%make-mml-value-parser $integer-parameter <sxmml-mml-transpose-change> rope->number))
(define-inline $transpose-command ($or $transpose
                                       ($seq ($. #\$) $transpose-env)
                                       ($seq ($. #\*) $transpose-change)))
(hash-table-set! *note-command* #\K $transpose-command)
(define-inline $tempo (%make-mml-value-parser $number-parameter <sxmml-mml-tempo> rope->number))
(hash-table-set! *note-command* #\t $tempo)
(define-inline $tempo-absolute (%make-mml-value-parser $number-parameter <sxmml-mml-tempo-absolute> rope->number))
(hash-table-set! *note-command* #\T $tempo-absolute)
(define-inline $detune (%make-mml-value-parser $integer-parameter <sxmml-mml-detune> rope->number))
(hash-table-set! *note-command* #\D $detune)
(define $loop
  (%$return
   ;; 反復は同じ位置を何度も通り、戻り先を一つに決められないので、L は反復の外にだけ書ける
   (begin (unless (null? (slot-ref (sxmml-mml-context) 'repeat-stack))
            (error "loop command cannot be used inside repeats"))
          (slot-set! (sxmml-mml-context) 'loop-exists #t)
          (let loop ([rest (slot-ref (sxmml-mml-context) 'current-ticks)]
                     [buf 0])
            (if (null? (cdr rest))
              (set! (cdar rest) (+ (caar rest) buf))
              (loop (cdr rest) (+ (caar rest) buf))))
          (make <sxmml-mml-loop>))))
(hash-table-set! *note-command* #\L $loop)

(define $repeat-start
  (%$return
   (begin (slot-push! (sxmml-mml-context) 'repeat-stack #f)
          (slot-set! (sxmml-mml-context) 'current-ticks
                     (acons 0 0 (slot-ref (sxmml-mml-context) 'current-ticks)))
          (make <sxmml-mml-repeat-start>))))
(hash-table-set! *note-command* #\[ $repeat-start)
(define $repeat-break
  (%$return
   ;; 反復の記録は [ で #f を積み、: でその位置の設定に置き換える。: は一つの反復に一つだけ
   (let1 stack (slot-ref (sxmml-mml-context) 'repeat-stack)
     (cond [(null? stack) (error "break command cannot be used outside of repeats")]
           [(car stack) (error "break command can be used only once in a repeat")])
     (let1 ct (car (slot-ref (sxmml-mml-context) 'current-ticks))
       (set! (cdr ct) (car ct)))
     (slot-pop! (sxmml-mml-context) 'repeat-stack)
     (slot-push! (sxmml-mml-context) 'repeat-stack
                 (list (slot-ref (sxmml-mml-context) 'default-length)
                       (slot-ref (sxmml-mml-context) 'current-octave)
                       (slot-ref (sxmml-mml-context) 'current-transpose)))
     (make <sxmml-mml-repeat-break>))))
(hash-table-set! *note-command* #\: $repeat-break)
(define $repeat-end
  ($let ([data $number-parameter]
         [$space])
        ($return (begin
                   (when (null? (slot-ref (sxmml-mml-context) 'repeat-stack))
                     (error "repeat end appeared before repeat start"))
                   (let* ([rc (rope->positive-number data "repeat count must be a positive exact integer: ")]
                          [ctx-values (slot-pop! (sxmml-mml-context) 'repeat-stack)]
                          [rt (slot-pop! (sxmml-mml-context) 'current-ticks)]
                          [ct (slot-ref (sxmml-mml-context) 'current-ticks)]
                          [cr (- (* (car rt) rc) (if ctx-values
                                                   (- (car rt) (cdr rt))
                                                   0))])
                     (set! (caar ct) (+ (caar ct) cr))
                     (when ctx-values
                       (slot-set! (sxmml-mml-context) 'default-length (car ctx-values))
                       (slot-set! (sxmml-mml-context) 'current-octave (cadr ctx-values))
                       (slot-set! (sxmml-mml-context) 'current-transpose (caddr ctx-values)))
                     (make <sxmml-mml-repeat-end> :value rc))))))
(hash-table-set! *note-command* #\] $repeat-end)

;; with parameter(ex. voice etc...)
(define-inline $voice (%make-mml-value-parser $parameter <sxmml-mml-voice> rope->symbol))
(hash-table-set! *note-command* #\@ $voice)
(define-inline $envelope (%make-mml-value-parser $parameter <sxmml-mml-envelope> rope->symbol))
(hash-table-set! *note-command* #\E $envelope)
(define $lfo-set
  ($let ([data ($or ($list ($seq $space ($. #\{) $space) ($many1 ($. #[^\s{}])) $space ($many1 ($. #[^\s{}])) ($seq $space ($. #\}) $space))
                    ($list $space $number ($return #t) ($return #f) $space))])
        (match data
          [(_ id _ ch _)
           ($return (make <sxmml-mml-lfo> :value (rope->symbol id) :channel (and ch (rope->symbol ch))))])))
(define $lfo-on
  ($let ([data ($optional $parameter)]
         [$space])
        ($return (make <sxmml-mml-lfo-switch> :value #t :channel (and data (rope->symbol data))))))
(define $lfo-off
  ($let ([data ($optional $parameter)]
         [$space])
        ($return (make <sxmml-mml-lfo-switch> :value #f :channel (and data (rope->symbol data))))))
(define-inline $lfo-command
  ($or $lfo-set
       ($seq ($. #\t) $lfo-on)
       ($seq ($. #\f) $lfo-off)))
(hash-table-set! *note-command* #\M $lfo-command)

;; (define $portamento
;;   ($let ([n1 ($seq ($. #\{) $space ($many ($or ($seq ($. #\o) $octave)
;;                                              ($seq ($. #\>) $octave-up)
;;                                              ($seq ($. #\<) $octave-down))) ($. #[a-g]))]
;;          [a1 ($many ($. #[-+]))]
;;          [$space]
;;          [n2 ($seq ($many ($or ($seq ($. #\o) $octave)
;;                                              ($seq ($. #\>) $octave-up)
;;                                              ($seq ($. #\<) $octave-down))) ($. #[a-g]))]
;;          [a2 ($many ($. #[-+]))]
;;          [($seq $space ($. #\}) $space)]
;;          [t $duration])
;;         (let ([nn1 (%proc-note-number #?=n1 a1)]
;;               [nn2 (%proc-note-number #?=n2 a2)])
;;           ($return (make <sxmml-mml-portamento> :value (cons nn1 nn2) :ticks t)))))
(define $portamento
  ($let ([n1 ($seq ($. #\{) $space ($many ($or ($seq ($. #\o) $octave)
                                             ($seq ($. #\>) $octave-up)
                                             ($seq ($. #\<) $octave-down))) ($. #[a-g]))]
         [a1 ($many ($. #[-+]))]
         [$space])
        (let ([nn1 (%proc-note-number n1 a1)])
          ($let
           ([n2 ($seq ($many ($or ($seq ($. #\o) $octave)
                                             ($seq ($. #\>) $octave-up)
                                             ($seq ($. #\<) $octave-down))) ($. #[a-g]))]
            [a2 ($many ($. #[-+]))]
            [($seq $space ($. #\}) $space)]
            [t $duration])
           (hash-table-update! (slot-ref (sxmml-mml-context) 'ticks-statistics) t (cut + 1 <>) 0)
           (let ([nn2 (%proc-note-number n2 a2)]
                 [ct (car (slot-ref (sxmml-mml-context) 'current-ticks))])
             (set! (car ct) (+ t (car ct)))
             ($return (make <sxmml-mml-portamento> :value (cons nn1 nn2) :ticks t)))))))
(hash-table-set! *note-command* #\/ $portamento)

(define-inline $noise-frequency (%make-mml-value-parser $number-parameter <sxmml-mml-noise-frequency> rope->number))
(hash-table-set! *note-command* #\w $noise-frequency)
(define-inline $output-switch (%make-mml-value-parser $number-parameter <sxmml-mml-output-switch> rope->number))
(hash-table-set! *note-command* #\P $output-switch)

(define-inline $note-commands
  ($seq $space ($or $note
                  ($let ([cmd ($any)]
                         [$space])
                        (hash-table-get *note-command* cmd)))))

;; rhythm command
(define (%make-rhythm-command-parser param class proc)
  ($let ([types $rhythm-type]
         [$space]
         [data param]
         [$space])
        (let ([args (%rhythm-types->initargs types (proc data))])
          ($return (apply make class args)))))

(define-inline (%rhythm-types->initargs types val)
  (fold (^[t r]
          (cons* (case t
                   ([#\b] :bass)
                   ([#\s] :snare)
                   ([#\t] :tom)
                   ([#\c] :cymbal)
                   ([#\h] :hihat)
                   ([#\i] :rimshot))
                 val
                 r))
        ()
        types))

(define-inline $rhythm-type ($or ($between ($seq ($. #\{) $space) ($many1 ($. #[bstchi])) ($seq $space ($. #\})))
                                 ($many1 ($. #[bstchi]))))
(define $rhythm-on
  ($let ([types $rhythm-type]
         [$space]
         [t $duration])
        (let ([args (%rhythm-types->initargs types #t)]
              [ct (car (slot-ref (sxmml-mml-context) 'current-ticks))])
          (set! (car ct) (+ t (car ct)))
          (hash-table-update! (slot-ref (sxmml-mml-context) 'ticks-statistics) t (cut + 1 <>) 0)
          ($return (apply make <sxmml-rhythm-emit> :value #t :ticks t args)))))
(hash-table-set! *rhythm-command* #\@ $rhythm-on)
(define $rhythm-off
  ($let ([types $rhythm-type]
         [$space])
        (let ([args (%rhythm-types->initargs types #t)])
          ($return (apply make <sxmml-rhythm-emit> :value #f args)))))
(hash-table-set! *rhythm-command* #\! $rhythm-off)
(define $rhythm-rest
  ($let ([t ($seq $duration)])
        (let1 ct (car (slot-ref (sxmml-mml-context) 'current-ticks))
          (set! (car ct) (+ t (car ct)))
          (hash-table-update! (slot-ref (sxmml-mml-context) 'ticks-statistics) t (cut + 1 <>) 0)
          ($return (make <sxmml-mml-rest> :ticks t)))))
(hash-table-set! *rhythm-command* #\r $rhythm-rest)
(define $rhythm-volume
  ($let ([param $number]
         [$space])
        ($return (make <sxmml-rhythm-volume> :value (rope->number param)))))
(hash-table-set! *rhythm-command* #\V $rhythm-volume)
(define-inline $rhythm-instrument-volume (%make-rhythm-command-parser $number <sxmml-rhythm-volume> rope->number))
(hash-table-set! *rhythm-command* #\v $rhythm-instrument-volume)
;; (define-inline $RHYTHM-INST-VOLUME-CHANGE
;;   ($or (%make-rhythm-command-parser ($. #\)) $integer <sxmml-rhythm-volume-change> rope->number)
;;        (%make-rhythm-command-parser ($. #\() $integer <sxmml-rhythm-volume-change> ($ change-sign $ rope->number $))
;;        (%make-rhythm-command-parser ($. "v*") $integer <sxmml-rhythm-volume-change> rope->number)))
(define-inline $rhythm-instrument-volume-up
  (%make-rhythm-command-parser $integer <sxmml-rhythm-volume-change> rope->number))
(hash-table-set! *rhythm-command* #\) $rhythm-instrument-volume-up)
(define-inline $rhythm-instrument-volume-down
  (%make-rhythm-command-parser $integer <sxmml-rhythm-volume-change> ($ change-sign $ rope->number $)))
(hash-table-set! *rhythm-command* #\( $rhythm-instrument-volume-down)

(define-inline $rhythm-pan (%make-rhythm-command-parser $number <sxmml-rhythm-pan> rope->number))
(hash-table-set! *rhythm-command* #\p $rhythm-pan)

(hash-table-set! *rhythm-command* #\L $loop)
(hash-table-set! *rhythm-command* #\[ $repeat-start)
(hash-table-set! *rhythm-command* #\: $repeat-break)
(hash-table-set! *rhythm-command* #\] $repeat-end)
(hash-table-set! *rhythm-command* #\l $length)

(define-inline $rhythm-commands
  ($seq $space ($let ([cmd ($any)]
                    [$space])
                   (hash-table-get *rhythm-command* cmd))))
