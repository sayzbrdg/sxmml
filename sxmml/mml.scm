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
  (export make-mml-parser make-rhythm-parser ;$NOTE-COMMANDS
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
(define-inline (rope->symbol rope)
  (string->symbol (rope->string rope)))

(define-inline (change-sign number)
  (* number -1))

(define-syntax $%return
  (er-macro-transformer
    (^[form rename id=?]
      (quasirename rename
        `(lambda [seq] (values #f ,@(cdr form) seq))))))

(define (mml-command->$ param class proc)
  ($let ([data param]
         [$SPC])
        ($return (make class :value (proc data)))))
(define (mml-command0->$ class)
  ($%return
   (make class)))
(define (mml-command*->$ param class proc slots)
  ($let ([data param]
         [$SPC])
        (let ([vals (fold (^[s d r]
                            (cons* s (proc d) r))
                          ()
                          slots
                          data)])
          ($return (apply make class vals)))))
(define (mml-env-command->$ param slot proc)
  ($let ([data param]
         [$SPC])
        (slot-set! (sxmml-mml-context) slot (proc data))
        ($return (make <sxmml-mml-nop>))))
(define (mml-env-command0->$ slot proc)
  ($%return
   (begin (slot-set! (sxmml-mml-context) slot (proc))
          (make <sxmml-mml-nop>))))

(define (make-mml-parser env)
  ($parameterize ([sxmml-mml-context env]) $NOTE-COMMANDS))

(define (make-rhythm-parser env)
  ($parameterize ([sxmml-mml-context env]) $RHYTHM-COMMANDS))

;; general value
(define-inline $SPC ($many_ ($. #[\s])))
(define-inline $NUMBER ($many1 ($. #[0-9])))
(define-inline $INTEGER ($list* ($optional ($. #[-+])) ($many1 ($. #[0-9]))))
(define-inline $PARAMETER ($or $NUMBER
                               ($between ($seq ($. #\{) $SPC) ($many1 ($. #[^\s{}])) ($seq $SPC ($. #\})))))
(define-inline $NUMPARAM ($or $NUMBER
                              ($between ($seq ($. #\{) $SPC) $NUMBER ($seq $SPC ($. #\})))))
(define-inline $INTPARAM ($or $INTEGER
                              ($between ($seq ($. #\{) $SPC) $INTEGER ($seq $SPC ($. #\})))))
(define-inline $MULTIPARAM ($between ($seq ($. #\{)) ($many1 ($seq $SPC ($many1 ($. #[^\s{}])))) ($seq $SPC ($. #\}))))


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

(define $notename
  ($let ([name ($. #[a-gr])]
         [accidentals ($many ($. #[-+]))]
         [$SPC])
        ($return (if (char=? name #\r)
                   (make <sxmml-mml-rest> :value #f)
                   (make <sxmml-mml-note> :value (%proc-note-number name accidentals))))))

(define (length->$ $number $dots)
  ($let ([number $number]
         [$SPC]
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

(define-inline $notelength ($or (length->$ $NUMBER ($many ($. #\.)))
                                (length->$ ($return #f) ($many1 ($. #\.)))))
(define-inline $notelength0 (length->$ ($optional $NUMBER) ($many ($. #\.))))
(define $tickslength ($let ([($. #\%)] [$SPC] [num $NUMBER] [$SPC]) ($return (rope->number num))))
(define $duration ($let ([f ($or $tickslength $notelength0)]
                         [l ($many ($try ($seq ($. #\&) $SPC ($or $tickslength $notelength))))])
                        ($return (apply + f l))))
(define $NOTE
  ($let ([o $notename]
         [t $duration]
         [$SPC])
        ($return
         (begin (slot-set! o 'ticks t)
                o))))

;; general command
(define-inline $LENGTH (mml-env-command->$ $NUMPARAM 'default-length rope->number))
(hash-table-set! *note-command* #\l $LENGTH)
(define-inline $OCTAVE (mml-env-command->$  $NUMPARAM 'current-octave rope->number))
(hash-table-set! *note-command* #\o $OCTAVE)
(define-inline $OCTAVE-UP
  (mml-env-command0->$ 'current-octave
                       (^[] (+ (slot-ref (sxmml-mml-context) 'current-octave) 1))))
(hash-table-set! *note-command* #\> $OCTAVE-UP)
(define-inline $OCTAVE-DOWN
  (mml-env-command0->$ 'current-octave
                       (^[] (- (slot-ref (sxmml-mml-context) 'current-octave) 1))))
(hash-table-set! *note-command* #\< $OCTAVE-DOWN)
(define-inline $VOLUME (mml-command->$ $NUMPARAM <sxmml-mml-volume> rope->number))
(define-inline $VOLUME-CHANGE (mml-command->$ $INTPARAM <sxmml-mml-volume-change> rope->number))
(define-inline $VOLUME-COMMAND ($or $VOLUME
                                    ($seq ($. #\*) $VOLUME-CHANGE)))
(hash-table-set! *note-command* #\v $VOLUME-COMMAND)
(define $VOLUME-UP
  ($%return
   (make <sxmml-mml-volume-change>
     :value (slot-ref (sxmml-mml-context) 'volume-delta))))
(hash-table-set! *note-command* #\) $VOLUME-UP)
(define $VOLUME-DOWN
  ($%return
   (make <sxmml-mml-volume-change>
     :value (change-sign (slot-ref (sxmml-mml-context) 'volume-delta)))))
(hash-table-set! *note-command* #\( $VOLUME-DOWN)
(define-inline $PAN (mml-command->$  $NUMPARAM <sxmml-mml-pan> rope->number))
(hash-table-set! *note-command* #\p $PAN)
(define-inline $GATETIME (mml-command->$ $NUMPARAM <sxmml-mml-gatetime> rope->number))
(hash-table-set! *note-command* #\q $GATETIME)
(define-inline $TIE (mml-command0->$ <sxmml-mml-tie>))
(hash-table-set! *note-command* #\& $TIE)
(define-inline $TRANSPOSE (mml-command->$ $INTPARAM <sxmml-mml-transpose> rope->number))
(define-inline $TRANSPOSE-ENV (mml-env-command->$ $INTPARAM 'current-transpose rope->number))
(define-inline $TRANSPOSE-CHANGE (mml-command->$ $INTPARAM <sxmml-mml-transpose-change> rope->number))
(define-inline $TRANSPOSE-COMMAND ($or $TRANSPOSE
                                       ($seq ($. #\$) $TRANSPOSE-ENV)
                                       ($seq ($. #\*) $TRANSPOSE-CHANGE)))
(hash-table-set! *note-command* #\K $TRANSPOSE-COMMAND)
(define-inline $TEMPO (mml-command->$ $NUMPARAM <sxmml-mml-tempo> rope->number))
(hash-table-set! *note-command* #\t $TEMPO)
(define-inline $TEMPO-ABSOLUTE (mml-command->$ $NUMPARAM <sxmml-mml-tempo-absolute> rope->number))
(hash-table-set! *note-command* #\T $TEMPO-ABSOLUTE)
(define-inline $DETUNE (mml-command->$ $INTPARAM <sxmml-mml-detune> rope->number))
(hash-table-set! *note-command* #\D $DETUNE)
(define $LOOP
  ($%return
   (make <sxmml-mml-loop>)))
(hash-table-set! *note-command* #\L $LOOP)

(define $REPEAT-START
  ($%return
   (begin (slot-push! (sxmml-mml-context) 'repeat-stack #f)
          (make <sxmml-mml-repeat-start>))))
(hash-table-set! *note-command* #\[ $REPEAT-START)
(define $REPEAT-BREAK
  ($%return
   (guard (e [else (error "break command cannot be used outside of repeats")])
     (slot-pop! (sxmml-mml-context) 'repeat-stack)
     (slot-push! (sxmml-mml-context) 'repeat-stack
                 (list (slot-ref (sxmml-mml-context) 'default-length)
                       (slot-ref (sxmml-mml-context) 'current-octave)
                       (slot-ref (sxmml-mml-context) 'current-transpose)))
     (make <sxmml-mml-repeat-break>))))
(hash-table-set! *note-command* #\: $REPEAT-BREAK)
(define $REPEAT-END
  ($let ([data $NUMPARAM]
         [$SPC])
        ($return (guard (e [else (error "repeat end appeared before repeat start")])
                   (let ([rc (rope->number data)]
                         [ctx-values (slot-pop! (sxmml-mml-context) 'repeat-stack)])
                     (when ctx-values
                       (slot-set! (sxmml-mml-context) 'default-length (car ctx-values))
                       (slot-set! (sxmml-mml-context) 'current-octave (cadr ctx-values))
                       (slot-set! (sxmml-mml-context) 'current-transpose (caddr ctx-values)))
                     (make <sxmml-mml-repeat-end> :value rc))))))
(hash-table-set! *note-command* #\] $REPEAT-END)

;; with parameter(ex. voice etc...)
(define-inline $VOICE (mml-command->$ $PARAMETER <sxmml-mml-voice> rope->symbol))
(hash-table-set! *note-command* #\@ $VOICE)
(define-inline $ENVELOPE (mml-command->$ $PARAMETER <sxmml-mml-envelope> rope->symbol))
(hash-table-set! *note-command* #\E $ENVELOPE)
(define $LFO-SET
  ($let ([data ($or ($list ($seq $SPC ($. #\{) $SPC) ($many1 ($. #[^\s{}])) $SPC ($many1 ($. #[^\s{}])) ($seq $SPC ($. #\}) $SPC))
                    ($list $SPC $NUMBER ($return #t) ($return #f) $SPC))])
        (match data
          [(_ id _ ch _)
           ($return (make <sxmml-mml-lfo> :value (rope->symbol id) :channel (and ch (rope->symbol ch))))])))
(define $LFO-ON
  ($let ([data ($optional $PARAMETER)]
         [$SPC])
        ($return (make <sxmml-mml-lfo-switch> :value #t :channel (and data (rope->symbol data))))))
(define $LFO-OFF
  ($let ([data ($optional $PARAMETER)]
         [$SPC])
        ($return (make <sxmml-mml-lfo-switch> :value #f :channel (and data (rope->symbol data))))))
(define-inline $LFO-COMMAND
  ($or $LFO-SET
       ($seq ($. #\t) $LFO-ON)
       ($seq ($. #\f) $LFO-OFF)))
(hash-table-set! *note-command* #\M $LFO-COMMAND)

;; (define $PORTAMENT
;;   ($let ([n1 ($seq ($. #\{) $SPC ($many ($or ($seq ($. #\o) $OCTAVE)
;;                                              ($seq ($. #\>) $OCTAVE-UP)
;;                                              ($seq ($. #\<) $OCTAVE-DOWN))) ($. #[a-g]))]
;;          [a1 ($many ($. #[-+]))]
;;          [$SPC]
;;          [n2 ($seq ($many ($or ($seq ($. #\o) $OCTAVE)
;;                                              ($seq ($. #\>) $OCTAVE-UP)
;;                                              ($seq ($. #\<) $OCTAVE-DOWN))) ($. #[a-g]))]
;;          [a2 ($many ($. #[-+]))]
;;          [($seq $SPC ($. #\}) $SPC)]
;;          [t $duration])
;;         (let ([nn1 (%proc-note-number #?=n1 a1)]
;;               [nn2 (%proc-note-number #?=n2 a2)])
;;           ($return (make <sxmml-mml-portament> :value (cons nn1 nn2) :ticks t)))))
(define $PORTAMENT
  ($let ([n1 ($seq ($. #\{) $SPC ($many ($or ($seq ($. #\o) $OCTAVE)
                                             ($seq ($. #\>) $OCTAVE-UP)
                                             ($seq ($. #\<) $OCTAVE-DOWN))) ($. #[a-g]))]
         [a1 ($many ($. #[-+]))]
         [$SPC])
        (let ([nn1 (%proc-note-number n1 a1)])
          ($let
           ([n2 ($seq ($many ($or ($seq ($. #\o) $OCTAVE)
                                             ($seq ($. #\>) $OCTAVE-UP)
                                             ($seq ($. #\<) $OCTAVE-DOWN))) ($. #[a-g]))]
            [a2 ($many ($. #[-+]))]
            [($seq $SPC ($. #\}) $SPC)]
            [t $duration])
           (let ([nn2 (%proc-note-number n2 a2)])
             ($return (make <sxmml-mml-portament> :value (cons nn1 nn2) :ticks t)))))))
(hash-table-set! *note-command* #\/ $PORTAMENT)

(define-inline $NOISEFREQ (mml-command->$ $NUMPARAM <sxmml-mml-noise-frequency> rope->number))
(hash-table-set! *note-command* #\w $NOISEFREQ)
(define-inline $OUTPUTSW (mml-command->$ $NUMPARAM <sxmml-mml-output-switch> rope->number))
(hash-table-set! *note-command* #\P $OUTPUTSW)

(define-inline $NOTE-COMMANDS
  ($seq $SPC ($or $NOTE
                  ($let ([cmd ($any)]
                         [$SPC])
                        (hash-table-get *note-command* cmd)))))

;; rhythm command
(define (rhythm-command->$ param class proc)
  ($let ([types $RHYTHM-TYPE]
         [$SPC]
         [data param]
         [$SPC])
        (let ([args (%rhythm-command-init-value-make types (proc data))])
          ($return (apply make class args)))))

(define-inline (%rhythm-command-init-value-make types val)
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

(define-inline $RHYTHM-TYPE ($or ($between ($seq ($. #\{) $SPC) ($many1 ($. #[bstchi])) ($seq $SPC ($. #\})))
                                 ($many1 ($. #[bstchi]))))
(define $RHYTHM-ON
  ($let ([types $RHYTHM-TYPE]
         [$SPC]
         [t $duration])
        (let ([args (%rhythm-command-init-value-make types #t)])
          ($return (apply make <sxmml-rhythm-emit> :value #t :ticks t args)))))
(hash-table-set! *rhythm-command* #\@ $RHYTHM-ON)
(define $RHYTHM-OFF
  ($let ([types $RHYTHM-TYPE]
         [$SPC])
        (let ([args (%rhythm-command-init-value-make types #t)])
          ($return (apply make <sxmml-rhythm-emit> :value #f args)))))
(hash-table-set! *rhythm-command* #\! $RHYTHM-OFF)
(define $RHYTHM-REST
  ($let ([t ($seq $duration)])
        ($return (make <sxmml-mml-rest> :ticks t))))
(hash-table-set! *rhythm-command* #\r $RHYTHM-REST)
(define $RHYTHM-VOLUME
  ($let ([param $NUMBER]
         [$SPC])
        ($return (make <sxmml-rhythm-volume> :value (rope->number param)))))
(hash-table-set! *rhythm-command* #\V $RHYTHM-VOLUME)
(define-inline $RHYTHM-INST-VOLUME (rhythm-command->$ $NUMBER <sxmml-rhythm-volume> rope->number))
(hash-table-set! *rhythm-command* #\v $RHYTHM-INST-VOLUME)
;; (define-inline $RHYTHM-INST-VOLUME-CHANGE
;;   ($or (rhythm-command->$ ($. #\)) $INTEGER <sxmml-rhythm-volume-change> rope->number)
;;        (rhythm-command->$ ($. #\() $INTEGER <sxmml-rhythm-volume-change> ($ change-sign $ rope->number $))
;;        (rhythm-command->$ ($. "v*") $INTEGER <sxmml-rhythm-volume-change> rope->number)))
(define-inline $RHYTHM-INST-VOLUME-UP
  (rhythm-command->$ $INTEGER <sxmml-rhythm-volume-change> rope->number))
(hash-table-set! *rhythm-command* #\) $RHYTHM-INST-VOLUME-UP)
(define-inline $RHYTHM-INST-VOLUME-DOWN
  (rhythm-command->$ $INTEGER <sxmml-rhythm-volume-change> ($ change-sign $ rope->number $)))
(hash-table-set! *rhythm-command* #\( $RHYTHM-INST-VOLUME-DOWN)

(define-inline $RHYTHM-PAN (rhythm-command->$ $NUMBER <sxmml-rhythm-pan> rope->number))
(hash-table-set! *rhythm-command* #\p $RHYTHM-PAN)

(hash-table-set! *rhythm-command* #\L $LOOP)
(hash-table-set! *rhythm-command* #\[ $REPEAT-START)
(hash-table-set! *rhythm-command* #\: $REPEAT-BREAK)
(hash-table-set! *rhythm-command* #\] $REPEAT-END)
(hash-table-set! *rhythm-command* #\l $LENGTH)

(define-inline $RHYTHM-COMMANDS
  ($seq $SPC ($let ([cmd ($any)]
                    [$SPC])
                   (hash-table-get *rhythm-command* cmd))))
