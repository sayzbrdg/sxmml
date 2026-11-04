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
  (use sxmml.command)
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
(define-inline (rope->positive-number rope :optional (message "invalid tick length: ~s (expected a positive exact integer)"))
  (let1 num (rope->number rope)
    (unless (and (exact-integer? num) (positive? num))
      (errorf <sxmml-mml-syntax-error> message num))
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
(define (%make-mml-context-value-parser param command proc)
  ($let ([data param]
         [$space])
        ($return (command (sxmml-mml-context) (proc data)))))

(define (%make-parser-with-context parser ctx)
  (let1 bound-parser ($parameterize ([sxmml-mml-context ctx]) parser)
    (^[seq]
      (guard (e [(sxmml-value-error? e)
                 (error <sxmml-mml-syntax-error> (condition-message e))])
        (bound-parser seq)))))

(define (make-sxmml-mml-parser ctx)
  (%make-parser-with-context $note-commands ctx))

(define (make-sxmml-rhythm-parser ctx)
  (%make-parser-with-context $rhythm-commands ctx))

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
  (sxmml-command-note-pitch
   (sxmml-mml-context)
   (case name
     [(#\c) 0] [(#\d) 2] [(#\e) 4] [(#\f) 5]
     [(#\g) 7] [(#\a) 9] [(#\b) 11])
   (fold (^[value result] (+ result (if (char=? value #\+) 1 -1)))
         0 accidentals)))

(define $note-name
  ($let ([name ($. #[a-gr])]
         [accidentals ($many ($. #[-+]))]
         [$space])
        ($return (and (not (char=? name #\r)) (%proc-note-number name accidentals)))))

(define (%make-note-length-parser $number $dots)
  ($let ([number $number]
         [$space]
         [dots $dots])
        ($return (sxmml-command-note-length->ticks
                  (sxmml-mml-context) (sxmml-resolution-get)
                  (rope->number number) (length dots)))))

(define-inline $note-length ($or (%make-note-length-parser $number ($many ($. #\.)))
                                (%make-note-length-parser ($return #f) ($many1 ($. #\.)))))
(define-inline $optional-note-length (%make-note-length-parser ($optional $number) ($many ($. #\.))))
(define $tick-length ($let ([($. #\%)] [$space] [num $number] [$space]) ($return (rope->positive-number num))))
(define $duration ($let ([f ($or $tick-length $optional-note-length)]
                         [l ($many ($try ($seq ($. #\&) $space ($or $tick-length $note-length))))])
                        ($return (apply + f l))))
(define $note
  ($let ([pitch $note-name]
         [ticks $duration]
         [$space])
        ($return (if pitch
                   (sxmml-command-note! (sxmml-mml-context) pitch ticks)
                   (sxmml-command-rest! (sxmml-mml-context) ticks)))))

;; general command
(define-inline $length
  (%make-mml-context-value-parser $number-parameter sxmml-command-default-length-set! rope->number))
(hash-table-set! *note-command* #\l $length)
(define-inline $octave
  (%make-mml-context-value-parser $number-parameter sxmml-command-octave-set! rope->number))
(hash-table-set! *note-command* #\o $octave)
(define-inline $octave-up
  (%$return (sxmml-command-octave-shift! (sxmml-mml-context) 1)))
(hash-table-set! *note-command* #\> $octave-up)
(define-inline $octave-down
  (%$return (sxmml-command-octave-shift! (sxmml-mml-context) -1)))
(hash-table-set! *note-command* #\< $octave-down)
(define-inline $volume (%make-mml-value-parser $number-parameter <sxmml-mml-volume> rope->number))
(define-inline $volume-change (%make-mml-value-parser $integer-parameter <sxmml-mml-volume-change> rope->number))
(define-inline $volume-command ($or $volume
                                    ($seq ($. #\*) $volume-change)))
(hash-table-set! *note-command* #\v $volume-command)
(define $volume-up
  (%$return (sxmml-command-volume-up (sxmml-mml-context))))
(hash-table-set! *note-command* #\) $volume-up)
(define $volume-down
  (%$return (sxmml-command-volume-down (sxmml-mml-context))))
(hash-table-set! *note-command* #\( $volume-down)
(define-inline $pan (%make-mml-value-parser  $number-parameter <sxmml-mml-pan> rope->number))
(hash-table-set! *note-command* #\p $pan)
(define-inline $gate-time (%make-mml-value-parser $number-parameter <sxmml-mml-gate-time> rope->number))
(hash-table-set! *note-command* #\q $gate-time)
(define-inline $tie (%make-mml-command-parser <sxmml-mml-tie>))
(hash-table-set! *note-command* #\& $tie)
(define-inline $transpose (%make-mml-value-parser $integer-parameter <sxmml-mml-transpose> rope->number))
(define-inline $transpose-env (%make-mml-context-value-parser $integer-parameter sxmml-command-note-transpose-set! rope->number))
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
  (%$return (sxmml-command-loop! (sxmml-mml-context))))
(hash-table-set! *note-command* #\L $loop)

(define $repeat-start
  (%$return (sxmml-command-repeat-start! (sxmml-mml-context))))
(hash-table-set! *note-command* #\[ $repeat-start)
(define $repeat-break
  (%$return (sxmml-command-repeat-break! (sxmml-mml-context))))
(hash-table-set! *note-command* #\: $repeat-break)
(define $repeat-end
  ($let ([data $number-parameter]
         [$space])
        ($return (sxmml-command-repeat-end! (sxmml-mml-context) (rope->number data)))))
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
           ($return (sxmml-command-portamento!
                     (sxmml-mml-context) nn1 (%proc-note-number n2 a2) t))))))
(hash-table-set! *note-command* #\/ $portamento)

(define-inline $noise-frequency (%make-mml-value-parser $number-parameter <sxmml-mml-noise-frequency> rope->number))
(hash-table-set! *note-command* #\w $noise-frequency)
(define-inline $output-switch (%make-mml-value-parser $number-parameter <sxmml-mml-output-switch> rope->number))
(hash-table-set! *note-command* #\P $output-switch)

(define-inline $note-commands
  ($seq $space ($or $note
                  ($let ([cmd ($any)]
                         [$space])
                        (guard (e [(is-a? e <error>)
                                   ($raise (format #f "unknown MML command: \"~a\"" cmd))])
                          (hash-table-get *note-command* cmd))))))

;; rhythm command
(define (%make-rhythm-command-parser param class proc)
  ($let ([types $rhythm-type]
         [$space]
         [data param]
         [$space])
        (let ([args (%rhythm-types->initargs types (proc data))])
          ($return (apply make class args)))))

(define (%rhythm-type->symbol type)
  (case type
    [(#\b) 'bass] [(#\s) 'snare] [(#\t) 'tom]
    [(#\c) 'cymbal] [(#\h) 'hihat] [(#\i) 'rimshot]))

(define-inline (%rhythm-types->initargs types val)
  (fold (^[type result]
          (cons* (make-keyword (%rhythm-type->symbol type)) val result))
        () types))

(define-inline $rhythm-type ($or ($between ($seq ($. #\{) $space) ($many1 ($. #[bstchi])) ($seq $space ($. #\})))
                                 ($many1 ($. #[bstchi]))))
(define $rhythm-on
  ($let ([types $rhythm-type]
         [$space]
         [ticks $duration])
        ($return (sxmml-command-rhythm-on!
                  (sxmml-mml-context) (map %rhythm-type->symbol types) ticks))))
(hash-table-set! *rhythm-command* #\@ $rhythm-on)
(define $rhythm-off
  ($let ([types $rhythm-type]
         [$space])
        (let ([args (%rhythm-types->initargs types #t)])
          ($return (apply make <sxmml-rhythm-emit> :value #f args)))))
(hash-table-set! *rhythm-command* #\! $rhythm-off)
(define $rhythm-rest
  ($let ([ticks $duration])
        ($return (sxmml-command-rest! (sxmml-mml-context) ticks))))
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
                   (guard (e [(is-a? e <error>)
                              ($raise (format #f "unknown MML command: \"~a\"" cmd))])
                     (hash-table-get *rhythm-command* cmd)))))
