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

(define-module sxmml.backend.pmd
  (use gauche.charconv)
  (use file.util)
  (use util.match)
  (use sxmml.classes)
  (use sxmml.environment)
  (use sxmml.compiler)
  (use sxmml.optimizer)
  (use sxmml.voice)
  (use sxmml.util)
  (export sxmml-pmd3-source-generate sxmml-pmd4-source-generate sxmml-pmd-source-generate
          pmd3-source pmd4-source pmd-source pmd3 pmd
          ))
(select-module sxmml.backend.pmd)

;;;
;;; mode
;;;
(define (pmd3-source)
  (slot-set! (sxmml-environment) 'backend sxmml-pmd3-source-generate)
  (slot-set! (sxmml-environment) 'mode "PMD3")
  (%pmd-source))
(define (pmd4-source)
  (slot-set! (sxmml-environment) 'backend sxmml-pmd4-source-generate)
  (slot-set! (sxmml-environment) 'mode "PMD4")
  (%pmd-source))
(define (pmd-source)
  (slot-set! (sxmml-environment) 'backend sxmml-pmd-source-generate)
  (slot-set! (sxmml-environment) 'mode "PMD")
  (%pmd-source))
(define pmd3 pmd3-source)
(define pmd pmd-source)

(define (%pmd-source)
  (sxmml-output-file (%output-file-get))
  (define-track (A B C D E F) :volume-delta 4)
  (define-track (G H I L))
  (define-track J :volume-delta 16))

;;;
;;; translate procedure
;;;
(define (sxmml-pmd3-source-generate)
  (%sxmml-pmdx-source-generate #t))

(define (sxmml-pmd4-source-generate)
  (%sxmml-pmdx-source-generate #f))

(define (sxmml-pmd-source-generate :optional (mode #f))
  (%sxmml-pmdx-source-generate mode))

(define (%sxmml-pmdx-source-generate mode)
  (%pmd-source-output mode)
  (sxmml-environment))

(define (%fm-track-generate track part)
  (let1 lis (sxmml-optimize-tree track :optimize (slot-ref (sxmml-environment) 'optimize))
    (unless (null? lis)
      (format #t "~a " part)
      (%pmd-track-generate lis
                           %pmd-fm-voice-set!
                           %pmd-fm-volume-set!
                           %pmd-fm-volume-change-set!)
      (display "\r\n\r\n"))))

(define (%ssg-track-generate track part)
  (let1 lis (sxmml-optimize-tree track :optimize (slot-ref (sxmml-environment) 'optimize))
    (unless (null? lis)
      (format #t "~a " part)
      (%pmd-track-generate lis
                           %pmd-ssg-voice-set!
                           %pmd-ssg-volume-set!
                           %pmd-ssg-volume-change-set!)
      (display "\r\n\r\n"))))

(define (%pcm-track-generate track part)
  (let1 lis (sxmml-optimize-tree track :optimize (slot-ref (sxmml-environment) 'optimize))
    (unless (null? lis)
      (format #t "~a " part)
      (%pmd-track-generate lis
                           %pmd-pcm-voice-set!
                           %pmd-pcm-volume-set!
                           %pmd-pcm-volume-change-set!)
      (display "\r\n\r\n"))))

(define (%pmd-track-generate lis voiceproc volproc volcproc)
  (let loop ([rest lis])
    (cond [(null? rest)
           ()]
          [(is-a? (car rest) <sxmml-mml-rest>)
           (%pmd-rest-set! (car rest))
           (loop (cdr rest))]
          [(is-a? (car rest) <sxmml-mml-portament>)
           (%pmd-portament-set! (car rest))
           (loop (cdr rest))]
          [(is-a? (car rest) <sxmml-mml-note>)
           (%pmd-note-set! (car rest))
           (loop (cdr rest))]
          [(is-a? (car rest) <sxmml-mml-voice>)
           (voiceproc (car rest))
           (loop (cdr rest) )]
          [(is-a? (car rest) <sxmml-mml-gatetime>)
           (%pmd-gatetime-set! (car rest))
           (loop (cdr rest))]
          [(is-a? (car rest) <sxmml-mml-volume>)
           (volproc (car rest))
           (loop (cdr rest))]
          [(is-a? (car rest) <sxmml-mml-tempo-absolute>)
           (%pmd-tempo-absolute-set! (car rest))
           (loop (cdr rest))]
          [(is-a? (car rest) <sxmml-mml-tempo>)
           (%pmd-tempo-set! (car rest))
           (loop (cdr rest))]
          [(is-a? (car rest) <sxmml-mml-tie>)
           (%pmd-tie-set! (car rest))
           (loop (cdr rest))]
          [(is-a? (car rest) <sxmml-mml-detune>)
           (%pmd-detune-set! (car rest))
           (loop (cdr rest))]
          [(is-a? (car rest) <sxmml-mml-repeat-start>)
           (%pmd-repeat-start-set! (car rest))
           (loop (cdr rest))]
          [(is-a? (car rest) <sxmml-mml-repeat-end>)
           (%pmd-repeat-end-set! (car rest))
           (loop (cdr rest))]
          [(is-a? (car rest) <sxmml-mml-repeat-break>)
           (%pmd-repeat-break-set! (car rest))
           (loop (cdr rest))]
          [(is-a? (car rest) <sxmml-mml-loop>)
           (%pmd-loop-set! (car rest))
           (loop (cdr rest))]
          [(is-a? (car rest) <sxmml-mml-transpose>)
           (%pmd-transpose-set! (car rest))
           (loop (cdr rest))]
          [(is-a? (car rest) <sxmml-mml-lfo-switch>)
           (%pmd-lfo-switch-set! (car rest))
           (loop (cdr rest))]
          [(is-a? (car rest) <sxmml-mml-lfo>)
           (%pmd-lfo-set! (car rest))
           (loop (cdr rest))]
          [(is-a? (car rest) <sxmml-mml-envelope>)
           (%pmd-envelope-set! (car rest))
           (loop (cdr rest))]
          [(is-a? (car rest) <sxmml-mml-pan>)
           (%pmd-pan-set! (car rest))
           (loop (cdr rest))]
          [(is-a? (car rest) <sxmml-mml-transpose-change>)
           (%pmd-transpose-change-set! (car rest))
           (loop (cdr rest))]
          [(is-a? (car rest) <sxmml-mml-volume-change>)
           (volcproc (car rest))
           (loop (cdr rest))]
          [(is-a? (car rest) <sxmml-mml-noise-frequency>)
           (%pmd-noise-freq-set! (car rest))
           (loop (cdr rest))]
          [(is-a? (car rest) <sxmml-mml-output-switch>)
           (%pmd-output-switch-set! (car rest))
           (loop (cdr rest))]
          [else
           (loop (cdr rest))])))

(define (%pmd-rhythm-track-generate track)
  (define lis (sxmml-optimize-tree track :optimize (slot-ref (sxmml-environment) 'optimize)))
  (define has-loop? (boolean (find (^o (is-a? o <sxmml-mml-loop>)) lis)))
  ;; R-channel
  (%pmd-rhythm-definition-track-generate has-loop? lis)
  ;; K-channel
  (%pmd-rhythm-deploy-track-generate has-loop? lis))

(define (%pmd-rhythm-definition-track-generate has-loop? lis)
  (define (loop rest)
    (cond [(null? rest)
           (display "\r\n")
           ()]
          [(is-a? (car rest) <sxmml-mml-loop>)
           (display "\r\n")
           (cdr rest)]
          [(is-a? (car rest) <sxmml-mml-rest>)
           (%pmd-rest-set! (car rest))
           (loop (cdr rest))]
          [(is-a? (car rest) <sxmml-rhythm-emit>)
           (%pmd-rhythm-emit-set! (car rest))
           (loop (cdr rest))]
          [(is-a? (car rest) <sxmml-mml-repeat-start>)
           (%pmd-repeat-start-set! (car rest))
           (loop (cdr rest))]
          [(is-a? (car rest) <sxmml-mml-repeat-end>)
           (%pmd-repeat-end-set! (car rest))
           (loop (cdr rest))]
          [(is-a? (car rest) <sxmml-mml-repeat-break>)
           (%pmd-repeat-break-set! (car rest))
           (loop (cdr rest))]
          [(is-a? (car rest) <sxmml-rhythm-volume>)
           (%pmd-rhythm-volume-set! (car rest))
           (loop (cdr rest))]
          [(is-a? (car rest) <sxmml-rhythm-volume-change>)
           (%pmd-rhythm-volume-change-set! (car rest))
           (loop (cdr rest))]
          [(is-a? (car rest) <sxmml-rhythm-pan>)
           (%pmd-rhythm-pan-set! (car rest))
           (loop (cdr rest))]
          [else
           (loop (cdr rest))]))
  (unless (null? lis)
    (display "R0 ")
    (let1 rest (loop lis)
      (when has-loop?
        (display "R1 ")
        (loop rest)))
    (display "\r\n")))

(define (%pmd-rhythm-deploy-track-generate has-loop? lis)
  (unless (null? lis)
    (display "K R0")
    (when has-loop?
      (display " L R1"))
    (display "\r\n\r\n")))

(define (%pmd-source-output :optional (pmd3? #f))
  (let1 output-file (%output-file-get)
    (call-with-temporary-file
        (^[p n]
          (with-output-conversion p
            (^[]
              (%pmd-source-head-generate pmd3?)
              (%pmd-source-meta-generate pmd3?)
              (%pmd-source-voice-generate)
              (for-each (^[t p] (%fm-track-generate t p)) (map sxmml-track-find '(A B C D E F)) '(A B C D E F))
              (for-each (^[t p] (%ssg-track-generate t p)) (map sxmml-track-find '(G H I)) '(G H I))
              (%pcm-track-generate (sxmml-track-find 'J) 'J)
              (%pmd-rhythm-track-generate (sxmml-track-find 'L))
              (when pmd3?
                (display #\x1a)))
            :encoding "cp932")
          (close-port p)
          (sys-rename n output-file))
      :directory (sys-dirname output-file))))

(define (%pmd-source-head-generate :optional (pmd3? #f))
  (format #t ";; This file was generated by sxmml from ~a\r\n;; mode: ~a\r\n\r\n"
          (sys-basename (sxmml-source-file)) (if pmd3? "PMDver3.3" "PMDver4.8")))

(define (%pmd-source-meta-generate :optional (pmd3? #f))
  (for-each (^[k] (let1 value (hash-table-get (slot-ref (sxmml-environment) 'metadata) k #f)
                    (when value
                      (when pmd3?
                        (display "; "))
                      (format #t "#~10a  ~a\r\n" k value))))
            '(title composer arranger pcmfile ppzfile ppsfile))
  (for-each (^[m] (when pmd3?
                    (display "; "))
              (format #t "#~10a  ~a\r\n" "memo" m))
            (hash-table-get (slot-ref (sxmml-environment) 'metadata) 'memo ()))
  (display "\r\n"))

(define (%pmd-source-voice-generate)
  (hash-table-for-each (slot-ref (sxmml-environment) 'voice)
    (^[key value]
      (let ([number (slot-ref value 'number)]
            [value (slot-ref value 'value)])
        (format #t "@~3,'0d" number)
        (match (car value)
          [(al fb)
           (format #t " ~3,'0d ~3,'0d\r\n" al fb)])
        (let loop ([rest (cdr value)])
          (unless (null? rest)
            (match (car rest)
              [(ar dr sr rr sl tl ks ml dt dt2 seg ams)
               (format #t " ~3,'0d ~3,'0d ~3,'0d ~3,'0d ~3,'0d ~3,'0d ~3,'0d ~3,'0d ~3,'0d ~3,'0d\r\n"
                       ar dr sr rr sl tl ks ml dt ams)
               (loop (cdr rest))]))))))
  (display "\r\n"))

;;;
;;; operators
;;;

;; PMD operator
(define (%pmd-rest-set! obj)
  (let recur ([ticks (slot-ref obj 'ticks)])
    (if (> ticks 255)
      (begin (format #t "r%255")
             (recur (- ticks 255)))
      (format #t "r%~d" ticks))))

(define (%pmd-note-set! obj)
  (let recur ([note (%note-number->pmd-note (slot-ref obj 'value))]
              [ticks (slot-ref obj 'ticks)])
    (format #t "o~d~a" (car note) (cdr note))
    (if (> ticks 255)
      (begin (format #t "%255&")
             (recur note (- ticks 255)))
      (format #t "%~d" ticks))))

(define (%pmd-fm-voice-set! obj)
  (let1 inst (hash-table-get (slot-ref (sxmml-environment) 'voice) (slot-ref obj 'value) #f)
    (unless inst
      (error "no instrument define: " (slot-ref obj 'value)))
    (format #t "@~d" (slot-ref inst 'number))))

(define (%pmd-gatetime-set! obj)
  (format #t "q~d" (slot-ref obj 'value)))

(define (%pmd-fm-volume-set! obj)
  (format #t "V~d" (slot-ref obj 'value)))

(define (%pmd-ssg-volume-set! obj)
  (format #t "v~d" (slot-ref obj 'value)))

(define (%pmd-pcm-volume-set! obj)
  (format #t "V~d" (slot-ref obj 'value)))

(define (%pmd-tempo-absolute-set! obj)
  (format #t "T~d" (slot-ref obj 'value)))

(define (%pmd-tempo-set! obj)
  (format #t "T~d" (bpm->timerb (slot-ref obj 'value))))

(define (%pmd-tie-set! obj)
  (format #t "&"))

(define (%pmd-detune-set! obj)
  (format #t "D~d" (slot-ref obj 'value)))

(define (%pmd-repeat-start-set! obj)
  (format #t "["))

(define (%pmd-repeat-end-set! obj)
  (format #t "]~d" (slot-ref obj 'value)))

(define (%pmd-repeat-break-set! obj)
  (format #t ":"))

(define (%pmd-loop-set! obj)
  (format #t "L"))

(define (%pmd-transpose-set! obj)
  (format #t "_~d" (slot-ref obj 'value)))

(define (%pmd-lfo-set! obj)
  (let1 lfo (hash-table-get (slot-ref (sxmml-environment) 'lfo) (slot-ref obj 'value) #f)
    (unless lfo
      (error "no lfo define: " (slot-ref obj 'value)))
    (let ([value (slot-ref lfo 'value)]
          [channel (slot-ref obj 'channel)])
      (when (or (not (= (length value) 4))
                (> (car value) 255) (< (car value) 0)
                (> (cadr value) 255) (< (cadr value) 0)
                (> (caddr value) 127) (< (caddr value) -128)
                (> (cadddr value) 255) (< (cadddr value) 0)
                (and (not (eq? channel 'A)) (not (eq? channel 'B)) (not (eq? channel #f))))
        (error "invalid lfo defition: " (slot-ref obj 'value)))
      (display "M")
      (when (eq? channel 'B)
        (display "B"))
      (format #t "~d,~d,~d,~d" (car value) (cadr value) (caddr value) (cadddr value)))))

(define (%pmd-lfo-switch-set! obj)
  (let ([channel (slot-ref obj 'channel)]
        [switch (slot-ref obj 'value)])
    (when (and (not (eq? channel 'A)) (not (eq? channel 'B)) (not (eq? channel #f)))
      (error "invalid lfo channel: " (slot-ref obj 'channel)))
    (display "*")
      (when (eq? channel 'B)
        (display "B"))
    (format #t "~d" (if switch 1 0))))

(define (%pmd-envelope-set! obj)
  (let1 envelope (hash-table-get (slot-ref (sxmml-environment) 'envelope) (slot-ref obj 'value) #f)
    (unless envelope
      (error "no envelope define: " (slot-ref obj 'value)))
    (let1 value (slot-ref envelope 'value)
      (when (cond [(= (length value) 4)
                   (or (> (car value) 255) (< (car value) 0)
                       (> (cadr value) 15) (< (cadr value) -15)
                       (> (caddr value) 255) (< (caddr value) 0)
                       (> (cadddr value) 255) (< (cadddr value) 0))]
                  [(<= (length value) 6)
                   (or (> (car value) 31) (< (car value) 0)
                       (> (cadr value) 31) (< (cadr value) 0)
                       (> (caddr value) 31) (< (caddr value) 0)
                       (> (cadddr value) 15) (< (cadddr value) 0)
                       (> (cadddr (cdr value)) 15) (< (cadddr (cdr value)) 0)
                       (and (= (length value) 6) (or (> (cadddr (cddr value)) 15) (< (cadddr (cddr value)) 0)))
                       )]
                  [else #t])
        (error "invalid envelope defition: " (slot-ref obj 'value)))
      (format #t "E~d,~d,~d,~d" (car value) (cadr value) (caddr value) (cadddr value))
      (when (= (length value) 6)
        (format #t ",~d,~d" (cadddr (cdr value)) (cadddr (cddr value)))))))

(define (%pmd-pan-set! obj)
  (format #t "p~d" (slot-ref obj 'value)))

(define (%pmd-transpose-change-set! obj)
  (format #t "__~d" (slot-ref obj 'value)))

(define (%pmd-fm-volume-change-set! obj)
  (let1 value (slot-ref obj 'value)
    (cond [(= value 4)
           (display ")")]
          [(= value -4)
           (display "(")]
          [(>= value 0)
           (format #t ")%~d" (slot-ref obj 'value))]
          [else
           (format #t "(%~d" (* (slot-ref obj 'value) -1))])))

(define (%pmd-ssg-volume-change-set! obj)
  (let1 value (slot-ref obj 'value)
    (cond [(= value 1)
           (display ")")]
          [(= value -1)
           (display "(")]
          [(>= value 0)
           (format #t ")%~d" (slot-ref obj 'value))]
          [else
           (format #t "(%~d" (* (slot-ref obj 'value) -1))])))

(define (%pmd-pcm-volume-change-set! obj)
  (let1 value (slot-ref obj 'value)
    (cond [(= value 16)
           (display ")")]
          [(= value -16)
           (display "(")]
          [(>= value 0)
           (format #t ")%~d" (slot-ref obj 'value))]
          [else
           (format #t "(%~d" (* (slot-ref obj 'value) -1))])))

(define (%pmd-portament-set! obj)
  (let ([start (%note-number->pmd-note (car (slot-ref obj 'value)))]
        [end (%note-number->pmd-note (cdr (slot-ref obj 'value)))]
        [ticks (slot-ref obj 'ticks)])
    (format #t "{o~d~ao~d~a}%~d" (car start) (cdr start) (car end) (cdr end) ticks)))

(define (%pmd-ssg-voice-set! obj)
  (let1 value (string->number (symbol->string (slot-ref obj 'value)))
    (unless (and (>= value 0) (< value 10))
      (error "invalid voice parameter: " value))
    (format #t "@~d" value)))

(define (%pmd-pcm-voice-set! obj)
  (let1 value (string->number (symbol->string (slot-ref obj 'value)))
    (unless (and (>= value 0) (< value 256))
      (error "invalid voice parameter: " value))
    (format #t "@~d" value)))

(define (%pmd-noise-freq-set! obj)
  (format #t "w~d" (slot-ref obj 'value)))

(define (%pmd-output-switch-set! obj)
  (format #t "P~d" (slot-ref obj 'value)))

;; Rhythm operator
(define (%pmd-rhythm-emit-set! obj)
  (let* ([b (if (slot-ref obj 'bass) (if (slot-ref obj 'value) "\\b" "\\bp") "")]
         [s (if (slot-ref obj 'snare) (if (slot-ref obj 'value) "\\s" "\\sp") "")]
         [t (if (slot-ref obj 'tom) (if (slot-ref obj 'value) "\\t" "\\tp") "")]
         [c (if (slot-ref obj 'cymbal) (if (slot-ref obj 'value) "\\c" "\\cp") "")]
         [h (if (slot-ref obj 'hihat) (if (slot-ref obj 'value) "\\h" "\\hp") "")]
         [i (if (slot-ref obj 'rimshot) (if (slot-ref obj 'value) "\\i" "\\ip") "")])
    (format #t "~a~a~a~a~a~a" b s t c h i)
    (when (slot-ref obj 'value)
      (%pmd-rest-set! (make <sxmml-mml-rest> :value #f :ticks (slot-ref obj 'ticks))))))

(define (%pmd-rhythm-volume-set! obj)
  (let ([v (if-let1 a (slot-ref obj 'value) (format "\\V~d" a) "")]
        [b (if-let1 a (slot-ref obj 'bass) (format "\\vb~d" a) "")]
        [s (if-let1 a (slot-ref obj 'snare) (format "\\vs~d" a) "")]
        [t (if-let1 a (slot-ref obj 'tom) (format "\\vt~d" a) "")]
        [c (if-let1 a (slot-ref obj 'cymbal) (format "\\vc~d" a) "")]
        [h (if-let1 a (slot-ref obj 'hihat) (format "\\vh~d" a) "")]
        [i (if-let1 a (slot-ref obj 'rimshot) (format "\\vi~d" a) "")])
    (format #t "~a~a~a~a~a~a~a" v b s t c h i)))

(define (%pmd-rhythm-volume-change-set! obj)
  (let ([b (if-let1 a (slot-ref obj 'bass) (format "\\vb~@d" a) "")]
        [s (if-let1 a (slot-ref obj 'snare) (format "\\vs~@d" a) "")]
        [t (if-let1 a (slot-ref obj 'tom) (format "\\vt~@d" a) "")]
        [c (if-let1 a (slot-ref obj 'cymbal) (format "\\vc~@d" a) "")]
        [h (if-let1 a (slot-ref obj 'hihat) (format "\\vh~@d" a) "")]
        [i (if-let1 a (slot-ref obj 'rimshot) (format "\\vi~@d" a) "")])
    (format #t "~a~a~a~a~a~a" b s t c h i)))

(define (%pmd-rhythm-pan-set! obj)
  (define (n->c num)
    (case num
      [(1) #\r]
      [(2) #\l]
      [(3) #\m]
      [(#f) #f]))
  (let ([b (if-let1 a (n->c (slot-ref obj 'bass)) (format "\\~ab" a) "")]
        [s (if-let1 a (n->c (slot-ref obj 'snare)) (format "\\~as" a) "")]
        [t (if-let1 a (n->c (slot-ref obj 'tom)) (format "\\~at" a) "")]
        [c (if-let1 a (n->c (slot-ref obj 'cymbal)) (format "\\~ac" a) "")]
        [h (if-let1 a (n->c (slot-ref obj 'hihat)) (format "\\~ah" a) "")]
        [i (if-let1 a (n->c (slot-ref obj 'rimshot)) (format "\\~ai" a) "")])
    (format #t "~a~a~a~a~a~a" b s t c h i)))

;; Misc
(define-inline (%note-number->pmd-note note)
  (receive (octave notenum) (quotient&remainder note 12)
    (cons octave (case notenum
                   [(0) "c"]
                   [(1) "c+"]
                   [(2) "d"]
                   [(3) "e-"]
                   [(4) "e"]
                   [(5) "f"]
                   [(6) "f+"]
                   [(7) "g"]
                   [(8) "a-"]
                   [(9) "a"]
                   [(10) "b-"]
                   [(11) "b"])
                       )))

(define (%output-file-get)
  (or (sxmml-metadata-get 'filename)
      (and-let* ([current-file (sxmml-source-file)])
        (path-swap-extension current-file "mml"))
      "./a.mml"))
