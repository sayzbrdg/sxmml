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

(define-module sxmml.backend.pmd-binary
  (use gauche.charconv)
  (use gauche.uvector)
  (use file.util)
  (use util.match)
  (use sxmml.classes)
  (use sxmml.environment)
  (use sxmml.compiler)
  (use sxmml.optimizer)
  (use sxmml.voice)
  (use sxmml.util)
  (export <sxmml-pmd-binary-environment>
          sxmml-pmd3-binary-generate sxmml-pmd4-binary-generate sxmml-pmd-binary-generate
          pmd3-binary pmd4-binary pmd-binary
          ))
(select-module sxmml.backend.pmd-binary)

;;;
;;; class
;;;
(define-class <sxmml-pmd-binary-environment> ()
  ([offset :init-keyword :offset] [mode :init-keyword :mode :init-value #f]
   [head :init-form (make-u8vector 26 0)] [head-length :init-keyword :head-length :init-value 26]
   [track :init-form (make-vector 13)] [track-length :init-form (make-vector 13 0)]
   [max-total-ticks :init-value 0] [max-loop-ticks :init-value 0]
   [used-fm-voices :init-form (make-hash-table eq-comparator)]))

(define-method initialize ([self <sxmml-pmd-binary-environment>] initargs)
  (next-method)
  (let1 track (slot-ref self 'track)
    (vector-for-each-with-index (^[i v] (vector-set! track i (make-u8vector 65600 0))) track)))

(define sxmml-pmd-binary-environment
  (make-parameter #f))

;;;
;;; mode
;;;
(define (pmd3-binary)
  (slot-set! (sxmml-environment) 'backend sxmml-pmd3-binary-generate)
  (slot-set! (sxmml-environment) 'mode "pmd3-binary")
  (%pmd-binary))
(define (pmd4-binary)
  (slot-set! (sxmml-environment) 'backend sxmml-pmd4-binary-generate)
  (slot-set! (sxmml-environment) 'mode "pmd4-binary")
  (%pmd-binary))
(define (pmd-binary)
  (slot-set! (sxmml-environment) 'backend sxmml-pmd-binary-generate)
  (slot-set! (sxmml-environment) 'mode "pmd-binary")
  (%pmd-binary))
(define (%pmd-binary)
  (slot-set! (sxmml-environment) 'output-extension "m")
  (define-track (A B C D E F) :volume-delta 4)
  (define-track (G H I L))
  (define-track J :volume-delta 16))

;;;
;;; translate procedure
;;;
(define (sxmml-pmd3-binary-generate)
  (%sxmml-pmdx-binary-generate 'fc88))

(define (sxmml-pmd4-binary-generate)
  (%sxmml-pmdx-binary-generate #f))

(define (sxmml-pmd-binary-generate :optional (mode #f))
  (%sxmml-pmdx-binary-generate mode))

(define (%sxmml-pmdx-binary-generate mode)
  (parameterize ([sxmml-pmd-binary-environment
                  (make <sxmml-pmd-binary-environment> :offset 26 :head-length 26 :mode mode)])
    (u8vector-set! (slot-ref (sxmml-pmd-binary-environment) 'head) 0 (slot-ref (sxmml-pmd-binary-environment) 'head-length))
    (for-each (^[t i h] (%fm-track-u8vector-set! i
                                                 h
                                                 (sxmml-optimize-tree t :optimize (slot-ref (sxmml-environment) 'optimize)))
                (%ticks-update! t))
              (map sxmml-track-find '(A B C D E F))
              (iota 6)
              (iota 6 2 2))
    (for-each (^[t i h] (%ssg-track-u8vector-set! i
                                                  h
                                                  (sxmml-optimize-tree t :optimize (slot-ref (sxmml-environment) 'optimize)))
                (%ticks-update! t))
              (map sxmml-track-find '(G H I))
              (iota 3 6)
              (iota 3 14 2))
    (let1 pcm-track (sxmml-track-find 'J)
      (%pcm-track-u8vector-set! 9 20 (sxmml-optimize-tree pcm-track :optimize (slot-ref (sxmml-environment) 'optimize)))
      (%ticks-update! pcm-track))
    (let1 rhythm-track (sxmml-track-find 'L)
      (%pmd-rhythm-track-u8vector-set! (sxmml-optimize-tree rhythm-track :optimize (slot-ref (sxmml-environment) 'optimize)))
      (%ticks-update! rhythm-track))
    (%pmd-meta-track-u8vector-set!)
    (%pmd-binary-output)
    (sxmml-environment)))


(define (%fm-track-u8vector-set! index hoffset lis)
  (%pmd-track-u8vector-set! index hoffset lis
                            %pmd-fm-voice-set!
                            %pmd-fm-volume-set!
                            %pmd-fm-volume-change-set!))

(define (%ssg-track-u8vector-set! index hoffset lis)
  (%pmd-track-u8vector-set! index hoffset lis
                            %pmd-ssg-voice-set!
                            %pmd-ssg-volume-set!
                            %pmd-ssg-volume-change-set!))

(define (%pcm-track-u8vector-set! index hoffset lis)
  (%pmd-track-u8vector-set! index hoffset lis
                            %pmd-pcm-voice-set!
                            %pmd-pcm-volume-set!
                            %pmd-pcm-volume-change-set!))

(define (%pmd-track-u8vector-set! index hoffset lis voiceproc volproc volcproc)
  (define u8vec (vector-ref (slot-ref (sxmml-pmd-binary-environment) 'track) index))
  (define overall-offset (slot-ref (sxmml-pmd-binary-environment) 'offset))
  (define repeat-stack ())
  (let loop ([rest lis]
             [track-offset 0])
    (cond [(null? rest)
           (u8vector-set! u8vec track-offset #x80)
           (vector-set! (slot-ref (sxmml-pmd-binary-environment) 'track-length) index (+ track-offset 1))
           (slot-set! (sxmml-pmd-binary-environment) 'offset (+ overall-offset track-offset 1))
           (receive (lb hb) (%short->le-bytes (slot-ref (sxmml-pmd-binary-environment) 'offset))
             (u8vector-set! (slot-ref (sxmml-pmd-binary-environment) 'head) hoffset lb)
             (u8vector-set! (slot-ref (sxmml-pmd-binary-environment) 'head) (+ hoffset 1) hb))
           (sxmml-pmd-binary-environment)]
          [(is-a? (car rest) <sxmml-mml-rest>)
           (loop (cdr rest) (%pmd-rest-set! u8vec track-offset (car rest)))]
          [(is-a? (car rest) <sxmml-mml-portament>)
           (loop (cdr rest) (%pmd-portament-set! u8vec track-offset (car rest)))]
          [(is-a? (car rest) <sxmml-mml-note>)
           (loop (cdr rest) (%pmd-note-set! u8vec track-offset (car rest)))]
          [(is-a? (car rest) <sxmml-mml-voice>)
           (loop (cdr rest) (voiceproc u8vec track-offset (car rest)))]
          [(is-a? (car rest) <sxmml-mml-gatetime>)
           (loop (cdr rest) (%pmd-gatetime-set! u8vec track-offset (car rest)))]
          [(is-a? (car rest) <sxmml-mml-volume>)
           (loop (cdr rest) (volproc u8vec track-offset (car rest)))]
          [(is-a? (car rest) <sxmml-mml-tempo-absolute>)
           (loop (cdr rest) (%pmd-tempo-absolute-set! u8vec track-offset (car rest)))]
          [(is-a? (car rest) <sxmml-mml-tempo>)
           (loop (cdr rest) (%pmd-tempo-set! u8vec track-offset (car rest)))]
          [(is-a? (car rest) <sxmml-mml-tie>)
           (loop (cdr rest) (%pmd-tie-set! u8vec track-offset (car rest)))]
          [(is-a? (car rest) <sxmml-mml-detune>)
           (loop (cdr rest) (%pmd-detune-set! u8vec track-offset (car rest)))]
          [(is-a? (car rest) <sxmml-mml-repeat-start>)
           (push! repeat-stack (cons (+ track-offset 1) 0))
           (loop (cdr rest) (%pmd-repeat-start-set! u8vec track-offset (car rest)))]
          [(is-a? (car rest) <sxmml-mml-repeat-end>)
           (loop (cdr rest) (%pmd-repeat-end-set! u8vec track-offset (car rest) (pop! repeat-stack) overall-offset))]
          [(is-a? (car rest) <sxmml-mml-repeat-break>)
           (set! (cdar repeat-stack) (+ track-offset 1))
           (loop (cdr rest) (%pmd-repeat-break-set! u8vec track-offset (car rest)))]
          [(is-a? (car rest) <sxmml-mml-loop>)
           (loop (cdr rest) (%pmd-loop-set! u8vec track-offset (car rest)))]
          [(is-a? (car rest) <sxmml-mml-transpose>)
           (loop (cdr rest) (%pmd-transpose-set! u8vec track-offset (car rest)))]
          [(is-a? (car rest) <sxmml-mml-lfo-switch>)
           (loop (cdr rest) (%pmd-lfo-switch-set! u8vec track-offset (car rest)))]
          [(is-a? (car rest) <sxmml-mml-lfo>)
           (loop (cdr rest) (%pmd-lfo-set! u8vec track-offset (car rest)))]
          [(is-a? (car rest) <sxmml-mml-envelope>)
           (loop (cdr rest) (%pmd-envelope-set! u8vec track-offset (car rest)))]
          [(is-a? (car rest) <sxmml-mml-pan>)
           (loop (cdr rest) (%pmd-pan-set! u8vec track-offset (car rest)))]
          [(is-a? (car rest) <sxmml-mml-transpose-change>)
           (loop (cdr rest) (%pmd-transpose-change-set! u8vec track-offset (car rest)))]
          [(is-a? (car rest) <sxmml-mml-volume-change>)
           (loop (cdr rest) (volcproc u8vec track-offset (car rest)))]
          [(is-a? (car rest) <sxmml-mml-noise-frequency>)
           (loop (cdr rest) (%pmd-noise-freq-set! u8vec track-offset (car rest)))]
          [(is-a? (car rest) <sxmml-mml-output-switch>)
           (loop (cdr rest) (%pmd-output-switch-set! u8vec track-offset (car rest)))]
          [else
           (loop (cdr rest) track-offset)])))

(define (%pmd-rhythm-track-u8vector-set! lis)
  (define has-loop? (boolean (find (^o (is-a? o <sxmml-mml-loop>)) lis)))
  ;; Track11(K-channel)
  (%pmd-rhythm-deploy-track-u8vector-set! has-loop? lis)
  (receive (lb hb) (%short->le-bytes (slot-ref (sxmml-pmd-binary-environment) 'offset))
    (u8vector-set! (slot-ref (sxmml-pmd-binary-environment) 'head) 22 lb)
    (u8vector-set! (slot-ref (sxmml-pmd-binary-environment) 'head) 23 hb))
  ;; Track12(R-channel)
  (%pmd-rhythm-definition-track-u8vector-set! has-loop? lis))

(define (%pmd-rhythm-deploy-track-u8vector-set! has-loop? lis)
  (define (overall-offset) (slot-ref (sxmml-pmd-binary-environment) 'offset))
  (define u8vec (vector-ref (slot-ref (sxmml-pmd-binary-environment) 'track) 10))
  (cond [(null? lis)
         (u8vector-set! u8vec 0 #x80)
         (vector-set! (slot-ref (sxmml-pmd-binary-environment) 'track-length) 10 1)
         (slot-set! (sxmml-pmd-binary-environment) 'offset (+ (overall-offset) 1))]
        [has-loop?
         (u8vector-set! u8vec 0 #x00)
         (u8vector-set! u8vec 1 #xf6)
         (u8vector-set! u8vec 2 #x01)
         (u8vector-set! u8vec 3 #x80)
         (vector-set! (slot-ref (sxmml-pmd-binary-environment) 'track-length) 10 4)
         (slot-set! (sxmml-pmd-binary-environment) 'offset (+ (overall-offset) 4))]
        [else
         (u8vector-set! u8vec 0 #x00)
         (u8vector-set! u8vec 1 #x80)
         (vector-set! (slot-ref (sxmml-pmd-binary-environment) 'track-length) 10 2)
         (slot-set! (sxmml-pmd-binary-environment) 'offset (+ (overall-offset) 2))]))

(define (%pmd-rhythm-definition-track-u8vector-set! has-loop? lis)
  (define (overall-offset) (slot-ref (sxmml-pmd-binary-environment) 'offset))
  (define u8vec (vector-ref (slot-ref (sxmml-pmd-binary-environment) 'track) 11))
  (define repeat-stack ())
  (let1 first-pat (if has-loop? 4 2)
    (receive (lb hb) (%short->le-bytes (+ (slot-ref (sxmml-pmd-binary-environment) 'offset) first-pat))
      (u8vector-set! u8vec 0 lb)
      (u8vector-set! u8vec 1 hb)))
  (let loop ([rest lis]
             [track-offset (if has-loop? 4 2)])
    (cond [(null? rest)
           (u8vector-set! u8vec track-offset #xff)
           ;; PMD4 is +12bytes to add meta informations.)
           (let1 metasize (if (eq? (slot-ref (sxmml-pmd-binary-environment) 'mode) 'fc88) 0 12)
             (vector-set! (slot-ref (sxmml-pmd-binary-environment) 'track-length) 11 (+ track-offset 1 metasize))
             (slot-set! (sxmml-pmd-binary-environment) 'offset (+ (overall-offset) track-offset 1 metasize))
             (receive (lb hb) (%short->le-bytes (slot-ref (sxmml-pmd-binary-environment) 'offset))
               (u8vector-set! (slot-ref (sxmml-pmd-binary-environment) 'head) 24 lb)
               (u8vector-set! (slot-ref (sxmml-pmd-binary-environment) 'head) 25 hb))
             (unless (eq? (slot-ref (sxmml-pmd-binary-environment) 'mode) 'fc88)
               (receive (lb hb) (%short->le-bytes (slot-ref (sxmml-pmd-binary-environment) 'max-total-ticks))
                 (u8vector-set! u8vec (+ track-offset 1) lb)
                 (u8vector-set! u8vec (+ track-offset 2) hb))
               (receive (lb hb) (%short->le-bytes (slot-ref (sxmml-pmd-binary-environment) 'max-loop-ticks))
                 (u8vector-set! u8vec (+ track-offset 5) lb)
                 (u8vector-set! u8vec (+ track-offset 6) hb))
               (u8vector-set! u8vec (- (+ track-offset 1 metasize) 2) #x48)
               (u8vector-set! u8vec (- (+ track-offset 1 metasize) 1) #xfe)))
           (sxmml-pmd-binary-environment)]
          [(is-a? (car rest) <sxmml-mml-loop>)
           (u8vector-set! u8vec track-offset #xff)
           (receive (lb hb) (%short->le-bytes (+ (overall-offset) track-offset 1))
             (u8vector-set! u8vec 2 lb)
             (u8vector-set! u8vec 3 hb))
           (loop (cdr rest) (+ track-offset 1))]
          [(is-a? (car rest) <sxmml-mml-rest>)
           (loop (cdr rest) (%pmd-rest-set! u8vec track-offset (car rest)))]
          [(is-a? (car rest) <sxmml-rhythm-emit>)
           (loop (cdr rest) (%pmd-rhythm-emit-set! u8vec track-offset (car rest)))]
          [(is-a? (car rest) <sxmml-mml-repeat-start>)
           (push! repeat-stack (cons (+ track-offset 1) 0))
           (loop (cdr rest) (%pmd-repeat-start-set! u8vec track-offset (car rest)))]
          [(is-a? (car rest) <sxmml-mml-repeat-end>)
           (loop (cdr rest) (%pmd-repeat-end-set! u8vec track-offset (car rest) (pop! repeat-stack) (overall-offset)))]
          [(is-a? (car rest) <sxmml-mml-repeat-break>)
           (set! (cdar repeat-stack) (+ track-offset 1))
           (loop (cdr rest) (%pmd-repeat-break-set! u8vec track-offset (car rest)))]
          [(is-a? (car rest) <sxmml-rhythm-volume>)
           (loop (cdr rest) (%pmd-rhythm-volume-set! u8vec track-offset (car rest)))]
          [(is-a? (car rest) <sxmml-rhythm-volume-change>)
           (loop (cdr rest) (%pmd-rhythm-volume-change-set! u8vec track-offset (car rest)))]
          [(is-a? (car rest) <sxmml-rhythm-pan>)
           (loop (cdr rest) (%pmd-rhythm-pan-set! u8vec track-offset (car rest)))]
          [else
           (loop (cdr rest) track-offset)])))


(define (%pmd-meta-track-u8vector-set!)
  (define voice-ht (slot-ref (sxmml-environment) 'voice))
  (define t12u8vec (vector-ref (slot-ref (sxmml-pmd-binary-environment) 'track) 11))
  (define t13u8vec (vector-ref (slot-ref (sxmml-pmd-binary-environment) 'track) 12))
  (define track-start-pos (slot-ref (sxmml-pmd-binary-environment) 'offset))
  (let1 track-offset
      (fold (^[value offset]
              (u8vector-set! t13u8vec offset (slot-ref value 'number))
              (u8vector-copy! t13u8vec (+ offset 1) (list->u8vector (%opn-voice-data-get (slot-ref value 'value))))
              (+ offset 26))
            0
            (sort (filter (cut hash-table-exists? (slot-ref (sxmml-pmd-binary-environment) 'used-fm-voices) <>)
                          (hash-table-values voice-ht))
                  ;; 同番号の音色も ID 順に並べ、音色表の反復順に依存させない。
                  (^[x y] (let ([nx (slot-ref x 'number)] [ny (slot-ref y 'number)])
                            (or (< nx ny)
                                (and (= nx ny)
                                     (string<? (symbol->string (slot-ref x 'id))
                                               (symbol->string (slot-ref y 'id)))))))))
    (if (eq? (slot-ref (sxmml-pmd-binary-environment) 'mode) 'fc88)
      (begin (vector-set! (slot-ref (sxmml-pmd-binary-environment) 'track-length) 12 track-offset)
             (slot-set! (sxmml-pmd-binary-environment) 'offset (+ track-start-pos track-offset)))
      (begin (u8vector-set! t13u8vec track-offset #x00)
             (u8vector-set! t13u8vec (+ track-offset 1) #xff)
             (vector-set! (slot-ref (sxmml-pmd-binary-environment) 'track-length) 12 (+ track-offset 2))
             (slot-set! (sxmml-pmd-binary-environment) 'offset (+ track-start-pos track-offset 2))))
    ;; for PMD4 meta informations.
    (unless (eq? (slot-ref (sxmml-pmd-binary-environment) 'mode) 'fc88)
      (let loop ([rest (list 'ppzfile 'ppsfile 'pcmfile 'title 'composer 'arranger 'memo)]
                 [meta-offset (+ track-offset 2)]
                 [offset-stack ()])
        (cond [(null? rest)
               ;; write pos info to the rhythm definition track
               (receive (lb hb) (%short->le-bytes (+ track-start-pos meta-offset))
                 (u8vector-set! t12u8vec (- (vector-ref (slot-ref (sxmml-pmd-binary-environment) 'track-length) 11) 4) lb)
                 (u8vector-set! t12u8vec (- (vector-ref (slot-ref (sxmml-pmd-binary-environment) 'track-length) 11) 3) hb))
               (let recur ([stack (reverse offset-stack)]
                           [toffset meta-offset])
                 (if (null? stack)
                   (begin (u8vector-set! t13u8vec toffset #x00)
                          (u8vector-set! t13u8vec (+ toffset 1) #x00)
                          (vector-set! (slot-ref (sxmml-pmd-binary-environment) 'track-length) 12 (+ toffset 2))
                          (slot-set! (sxmml-pmd-binary-environment) 'offset (+ track-start-pos toffset 2))
                          (+ toffset 2))
                   (receive (lb hb) (%short->le-bytes (+ track-start-pos (car stack)))
                     (u8vector-set! t13u8vec toffset lb)
                     (u8vector-set! t13u8vec (+ toffset 1) hb)
                     (recur (cdr stack) (+ toffset 2)))))]
              [(eq? (car rest) 'memo)
               (let1 data (hash-table-get (slot-ref (sxmml-environment) 'metadata) (car rest) #f)
                 (cond [(pair? data)
                        (let recur ([stack data]
                                    [moffset meta-offset]
                                    [ostack offset-stack])
                          (if (null? stack)
                            (loop (cdr rest) moffset ostack)
                            (let1 doffset (%shift_jis-u8vector-copy! t13u8vec (car stack) moffset)
                              (recur (cdr stack) doffset (cons moffset ostack)))))]
                       [data
                        (let1 doffset (%shift_jis-u8vector-copy! t13u8vec data meta-offset)
                          (loop (cdr rest) doffset (cons meta-offset offset-stack)))]
                       [else
                        (u8vector-set! t13u8vec meta-offset #x00)
                        (loop (cdr rest) (+ meta-offset 1) (cons meta-offset offset-stack))]))]
              [else
               (if-let1 data (hash-table-get (slot-ref (sxmml-environment) 'metadata) (car rest) #f)
                 (let1 doffset (%shift_jis-u8vector-copy! t13u8vec data meta-offset)
                   (loop (cdr rest) doffset (cons meta-offset offset-stack)))
                 (begin (u8vector-set! t13u8vec meta-offset #x00)
                        (loop (cdr rest) (+ meta-offset 1) (cons meta-offset offset-stack))))])))))


(define (%pmd-binary-output)
  (%check-obsolete-metadata)
  (let1 output-file (%output-file-get)
    (call-with-temporary-file
        (^[p n]
          (display #\null p)
          (write-uvector (slot-ref (sxmml-pmd-binary-environment) 'head) p 0
                         (slot-ref (sxmml-pmd-binary-environment) 'head-length))
          (vector-for-each (^[uvec len]
                             (write-uvector uvec p 0 len))
                           (slot-ref (sxmml-pmd-binary-environment) 'track)
                           (slot-ref (sxmml-pmd-binary-environment) 'track-length))
          (close-port p)
          (sys-rename n output-file))
      :directory (sys-dirname output-file))
    output-file))


;;;
;;; operators
;;;

;; PMD operator
(define (%pmd-rest-set! u8vec offset obj)
  (let recur ([ticks (slot-ref obj 'ticks)]
              [cur offset])
    (u8vector-set! u8vec cur #x0f)
    (if (> ticks 255)
      (begin (u8vector-set! u8vec (+ cur 1) #xff)
             (recur (- ticks 255) (+ cur 2)))
      (begin (u8vector-set! u8vec (+ cur 1) ticks)
             (+ cur 2)))))

(define (%pmd-note-set! u8vec offset obj)
  (let recur ([note (%note-number->pmd-note (slot-ref obj 'value))]
              [ticks (slot-ref obj 'ticks)]
              [cur offset])
    (u8vector-set! u8vec cur note)
    (if (> ticks 255)
      (begin (u8vector-set! u8vec (+ cur 1) #xff)
             (u8vector-set! u8vec (+ cur 2) #xfb)
             (recur note (- ticks 255) (+ cur 3)))
      (begin (u8vector-set! u8vec (+ cur 1) ticks)
             (+ cur 2)))))

(define (%pmd-fm-voice-set! u8vec offset obj)
  (let1 inst (hash-table-get (slot-ref (sxmml-environment) 'voice) (slot-ref obj 'value) #f)
    (unless inst
      (error "no instrument define: " (slot-ref obj 'value)))
    (hash-table-put! (slot-ref (sxmml-pmd-binary-environment) 'used-fm-voices) inst #t)
    (%pmd-1byte-set! u8vec offset #xff (slot-ref inst 'number) 255 0)))

(define (%pmd-gatetime-set! u8vec offset obj)
  (%pmd-1byte-set! u8vec offset #xfe (slot-ref obj 'value) 255 0))

(define (%pmd-fm-volume-set! u8vec offset obj)
  (%pmd-1byte-set! u8vec offset #xfd (slot-ref obj 'value) 127 0))

(define (%pmd-ssg-volume-set! u8vec offset obj)
  (%pmd-1byte-set! u8vec offset #xfd (slot-ref obj 'value) 15 0))

(define (%pmd-pcm-volume-set! u8vec offset obj)
  (%pmd-1byte-set! u8vec offset #xfd (slot-ref obj 'value) 255 0))

(define (%pmd-tempo-absolute-set! u8vec offset obj)
  (%pmd-1byte-set! u8vec offset #xfc (slot-ref obj 'value) 250 0))

(define (%pmd-tempo-set! u8vec offset obj)
  (%pmd-1byte-set! u8vec offset #xfc (bpm->timerb (slot-ref obj 'value)) 250 0))
  ;; (when (or (> value 255) (< value 18))
  ;;   (errorf "value exceeds the range: ~a (~a - ~a): ~a" value 18 255 obj))
  ;; (u8vector-set! u8vec offset #xfc)
  ;; (u8vector-set! u8vec (+ offset 1) #xff)
  ;; (u8vector-set! u8vec (+ offset 2) value)
  ;; (+ offset 3))

(define (%pmd-tie-set! u8vec offset obj)
  (%pmd-single-set! u8vec offset #xfb))

(define (%pmd-detune-set! u8vec offset obj)
  (%pmd-2bytes-set! u8vec offset #xfa (slot-ref obj 'value) 32767 -32768))

(define (%pmd-repeat-start-set! u8vec offset obj)
  (%pmd-2bytes-set! u8vec offset #xf9 0 0 0))

(define (%pmd-repeat-end-set! u8vec offset obj addr overall)
  (receive (lb hb) (%short->le-bytes (+ overall offset 1))
    (u8vector-set! u8vec (car addr) lb)
    (u8vector-set! u8vec (+ (car addr) 1) hb)
    (unless (zero? (cdr addr))
      (u8vector-set! u8vec (cdr addr) lb)
      (u8vector-set! u8vec (+ (cdr addr) 1) hb)))
  (let1 repcnt (slot-ref obj 'value)
    (when (> repcnt 255)
      (errorf "value exceeds the max: ~a (>255): ~a" repcnt obj))
    (u8vector-set! u8vec offset #xf8)
    (u8vector-set! u8vec (+ offset 1) repcnt)
    (u8vector-set! u8vec (+ offset 2) (if (zero? (cdr addr))
                                        repcnt
                                        (- repcnt 1)))
    (receive (lb hb) (%short->le-bytes (+ overall (car addr)))
      (u8vector-set! u8vec (+ offset 3) lb)
      (u8vector-set! u8vec (+ offset 4) hb)
      (+ offset 5))))

(define (%pmd-repeat-break-set! u8vec offset obj)
  (%pmd-2bytes-set! u8vec offset #xf7 0 0 0))

(define (%pmd-loop-set! u8vec offset obj)
  (%pmd-single-set! u8vec offset #xf6))

(define (%pmd-transpose-set! u8vec offset obj)
  (%pmd-1byte-set! u8vec offset #xf5 (slot-ref obj 'value) 127 -128))

(define (%pmd-lfo-set! u8vec offset obj)
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
                (and (not (eq? channel 'A)) (not (eq? channel 'B)) (not (eq? channel #f)))
                (and (eq? (slot-ref (sxmml-pmd-binary-environment) 'mode) 'fc88) (eq? channel 'B)))
        (error "invalid lfo defition: " (slot-ref obj 'value)))
      (u8vector-set! u8vec offset (if (or (eq? channel #f) (eq? channel 'A)) #xf2 #xbf))
      (u8vector-set! u8vec (+ offset 1) (car value))
      (u8vector-set! u8vec (+ offset 2) (cadr value))
      (u8vector-set! u8vec (+ offset 3) (logand (caddr value) #xff))
      (u8vector-set! u8vec (+ offset 4) (cadddr value))
      (+ offset 5))))

(define (%pmd-lfo-switch-set! u8vec offset obj)
  (let ([channel (slot-ref obj 'channel)]
        [switch (slot-ref obj 'value)])
    (when (or (and (not (eq? channel 'A)) (not (eq? channel 'B)) (not (eq? channel #f)))
              (and (eq? (slot-ref (sxmml-pmd-binary-environment) 'mode) 'fc88) (eq? channel 'B)))
        (error "invalid lfo channel: " (slot-ref obj 'channel)))
    (u8vector-set! u8vec offset (if (or (eq? channel #f) (eq? channel 'A)) #xf1 #xbe))
    (u8vector-set! u8vec (+ offset 1) (if switch 1 0))
    (+ offset 2)))

(define (%pmd-envelope-set! u8vec offset obj)
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
                       (eq? (slot-ref (sxmml-pmd-binary-environment) 'mode) 'fc88))]
                  [else #t])
        (error "invalid envelope defition: " (slot-ref obj 'value)))
      (if (= (length value) 4)
        (begin (u8vector-set! u8vec offset #xf0)
               (u8vector-set! u8vec (+ offset 1) (car value))
               (u8vector-set! u8vec (+ offset 2) (logand (cadr value) #xff))
               (u8vector-set! u8vec (+ offset 3) (caddr value))
               (u8vector-set! u8vec (+ offset 4) (cadddr value))
               (+ offset 5))
        (begin (u8vector-set! u8vec offset #xcd)
               (u8vector-set! u8vec (+ offset 1) (car value))
               (u8vector-set! u8vec (+ offset 2) (cadr value))
               (u8vector-set! u8vec (+ offset 3) (caddr value))
               (u8vector-set! u8vec (+ offset 4) (logior (cadddr value) (ash (cadddr (cdr value)) 4)))
               (u8vector-set! u8vec (+ offset 5) (if (= (length value) 6) (cadddr (cddr value)) 0))
               (+ offset 6))))))

(define (%pmd-pan-set! u8vec offset obj)
  (%pmd-1byte-set! u8vec offset #xec (slot-ref obj 'value) 3 0))

(define (%pmd-transpose-change-set! u8vec offset obj)
  (%pmd-1byte-set! u8vec offset #xe7 (slot-ref obj 'value) 127 -128))

(define (%pmd-fm-volume-change-set! u8vec offset obj)
  (let1 value (slot-ref obj 'value)
    (cond [(= value 4)
           (%pmd-single-set! u8vec offset #xf4)]
          [(= value -4)
           (%pmd-single-set! u8vec offset #xf3)]
          [(>= value 0)
           (%pmd-1byte-set! u8vec offset #xe3 (slot-ref obj 'value) 127 0)]
          [else
           (%pmd-1byte-set! u8vec offset #xe2 (* (slot-ref obj 'value) -1) 127 0)])))

(define (%pmd-ssg-volume-change-set! u8vec offset obj)
  (let1 value (slot-ref obj 'value)
    (cond [(= value 1)
           (%pmd-single-set! u8vec offset #xf4)]
          [(= value -1)
           (%pmd-single-set! u8vec offset #xf3)]
          [(>= value 0)
           (%pmd-1byte-set! u8vec offset #xe3 (slot-ref obj 'value) 15 0)]
          [else
           (%pmd-1byte-set! u8vec offset #xe2 (* (slot-ref obj 'value) -1) 15 0)])))

(define (%pmd-pcm-volume-change-set! u8vec offset obj)
  (let1 value (slot-ref obj 'value)
    (cond [(= value 16)
           (%pmd-single-set! u8vec offset #xf4)]
          [(= value -16)
           (%pmd-single-set! u8vec offset #xf3)]
          [(>= value 0)
           (%pmd-1byte-set! u8vec offset #xe3 (slot-ref obj 'value) 255 0)]
          [else
           (%pmd-1byte-set! u8vec offset #xe2 (* (slot-ref obj 'value) -1) 255 0)])))

(define (%pmd-portament-set! u8vec offset obj)
  (let ([start (%note-number->pmd-note (car (slot-ref obj 'value)))]
        [end (%note-number->pmd-note (cdr (slot-ref obj 'value)))]
        [ticks (slot-ref obj 'ticks)])
    (u8vector-set! u8vec offset #xda)
    (u8vector-set! u8vec (+ offset 1) start)
    (u8vector-set! u8vec (+ offset 2) end)
    (u8vector-set! u8vec (+ offset 3) ticks)
    (+ offset 4)))

(define (%pmd-ssg-voice-set! u8vec offset obj)
  (define (data-set! p1 p2 p3 p4)
    (u8vector-set! u8vec offset #xf0)
    (u8vector-set! u8vec (+ offset 1) p1)
    (u8vector-set! u8vec (+ offset 2) (logand p2 #xff))
    (u8vector-set! u8vec (+ offset 3) p3)
    (u8vector-set! u8vec (+ offset 4) p4)
    (+ offset 5))
  (let1 value (slot-ref obj 'value)
    (case value
      [(|0|) (data-set! 0 0 0 0)]
      [(|1|) (data-set! 2 -1 0 1)]
      [(|2|) (data-set! 2 -2 0 1)]
      [(|3|) (data-set! 2 -2 0 8)]
      [(|4|) (data-set! 2 -1 24 1)]
      [(|5|) (data-set! 2 -2 24 1)]
      [(|6|) (data-set! 2 -2 4 1)]
      [(|7|) (data-set! 2 1 0 1)]
      [(|8|) (data-set! 1 2 0 1)]
      [(|9|) (data-set! 1 2 24 1)]
      [else
       (error "invalid voice parameter: " value)])))

(define (%pmd-pcm-voice-set! u8vec offset obj)
  (let1 value (string->number (symbol->string (slot-ref obj 'value)))
    (unless (and value
                 (positive? value)
                 (integer? value)
                 (exact? value))
      (error "invalid voice paramater: " value))
    (%pmd-1byte-set! u8vec offset #xff value 255 0)))

(define (%pmd-noise-freq-set! u8vec offset obj)
  (%pmd-1byte-set! u8vec offset #xee (slot-ref obj 'value) 31 0))

(define (%pmd-output-switch-set! u8vec offset obj)
  (let1 value (slot-ref obj 'value)
    (when (or (> value 3) (< value 1))
      (errorf "value exceeds the range: ~a (~a - ~a): ~a" value min max #xed))
    (u8vector-set! u8vec offset #xed)
    (u8vector-set! u8vec (+ offset 1) (case value ([1] #x07) ([2] #x38) ([3] #x3f)))
    (+ offset 2)))

;; Rhythm operator
(define (%pmd-rhythm-emit-set! u8vec offset obj)
  (let* ([sw (if (slot-ref obj 'value) 0 128)]
         [b (if (slot-ref obj 'bass) 1 0)]
         [s (if (slot-ref obj 'snare) 2 0)]
         [t (if (slot-ref obj 'tom) 16 0)]
         [c (if (slot-ref obj 'cymbal) 4 0)]
         [h (if (slot-ref obj 'hihat) 8 0)]
         [i (if (slot-ref obj 'rimshot) 32 0)]
         [cur (%pmd-1byte-set! u8vec offset #xeb (+ sw b s t c h i) 255 1)])
    (if (slot-ref obj 'value)
      (%pmd-rest-set! u8vec cur (make <sxmml-mml-rest> :value #f :ticks (slot-ref obj 'ticks)))
      cur)))

(define (%pmd-rhythm-volume-set! u8vec offset obj)
  (define (data-set! offset cmd value max)
    (when (or (and (= cmd #xe8) (> value max))
              (< value 0))
      (error "invalid rhythm volume paramater: " value))
    (u8vector-set! u8vec offset cmd)
    (u8vector-set! u8vec (+ offset 1) value)
    (+ offset 2))
  (let ([v (cons 0 (slot-ref obj 'value))]
        [b (cons 1 (slot-ref obj 'bass))]
        [s (cons 2 (slot-ref obj 'snare))]
        [t (cons 5 (slot-ref obj 'tom))]
        [c (cons 3 (slot-ref obj 'cymbal))]
        [h (cons 4 (slot-ref obj 'hihat))]
        [i (cons 6 (slot-ref obj 'rimshot))])
    (let recur ([rest (list v b s t c h i)]
                [cur offset])
      (cond [(null? rest)
             cur]
            [(and (= (caar rest) 0) (cdar rest))
             (recur (cdr rest) (data-set! cur #xe8 (cdar rest) 63))]
            [(cdar rest)
             (recur (cdr rest) (data-set! cur #xea (logior (ash (caar rest) 5) (cdar rest)) 31))]
            [else
             (recur (cdr rest) cur)]))))

(define (%pmd-rhythm-volume-change-set! u8vec offset obj)
  (define (data-set! offset inst value)
    (when (or (> value 31) (< value -31))
      (error "invalid rhythm volume paramater: " value))
    (u8vector-set! u8vec offset #xe5)
    (u8vector-set! u8vec (+ offset 1) inst)
    (u8vector-set! u8vec (+ offset 2) (logand value #xff))
    (+ offset 3))
  (let ([b (cons 1 (slot-ref obj 'bass))]
        [s (cons 2 (slot-ref obj 'snare))]
        [t (cons 5 (slot-ref obj 'tom))]
        [c (cons 3 (slot-ref obj 'cymbal))]
        [h (cons 4 (slot-ref obj 'hihat))]
        [i (cons 6 (slot-ref obj 'rimshot))])
    (let recur ([rest (list b s t c h i)]
                [cur offset])
      (cond [(null? rest)
             cur]
            [(cdar rest)
             (recur (cdr rest) (data-set! cur (caar rest) (cdar rest)))]
            [else
             (recur (cdr rest) cur)]))))

(define (%pmd-rhythm-pan-set! u8vec offset obj)
  (define (data-set! offset value)
    (when (> (logand value #x1f) 3)
      (error "invalid rhythm pan paramater: " value))
    (u8vector-set! u8vec offset #xe9)
    (u8vector-set! u8vec (+ offset 1) value)
    (+ offset 2))
  (let ([b (cons 1 (slot-ref obj 'bass))]
        [s (cons 2 (slot-ref obj 'snare))]
        [t (cons 5 (slot-ref obj 'tom))]
        [c (cons 3 (slot-ref obj 'cymbal))]
        [h (cons 4 (slot-ref obj 'hihat))]
        [i (cons 6 (slot-ref obj 'rimshot))])
    (let recur ([rest (list b s t c h i)]
                [cur offset])
      (cond [(null? rest)
             cur]
            [(cdar rest)
             (recur (cdr rest) (data-set! cur (logior (ash (caar rest) 5) (cdar rest))))]
            [else
             (recur (cdr rest) cur)]))))


;; Misc
(define-inline (%pmd-single-set! u8vec offset cmd)
  (u8vector-set! u8vec offset cmd)
  (+ offset 1))

(define-inline (%pmd-1byte-set! u8vec offset cmd value max min)
  (when (or (> value max) (< value min))
    (errorf "value exceeds the range: ~a (~a - ~a): ~a" value min max cmd))
  (u8vector-set! u8vec offset cmd)
  (u8vector-set! u8vec (+ offset 1) (logand value #xff))
  (+ offset 2))

(define-inline (%pmd-2bytes-set! u8vec offset cmd value max min)
  (when (or (> value max) (< value min))
    (errorf "value exceeds the range: ~a (~a - ~a): ~a" value min max cmd))
  (receive (lb hb) (%short->le-bytes value)
    (u8vector-set! u8vec offset cmd)
    (u8vector-set! u8vec (+ offset 1) lb)
    (u8vector-set! u8vec (+ offset 2) hb)
    (+ offset 3)))

(define-inline (%short->le-bytes shortint)
  (values (logand #b11111111 shortint) (ash (logand #b1111111100000000 shortint) -8)))

;; PMD の音高の 1 バイトは、上位 4 ビットがオクターブ - 1、下位 4 ビットが音名で、
;; o1c から o8b までを表す
(define-inline (%note-number->pmd-note note)
  (unless (<= 12 note 107)
    (errorf "note out of range: ~a (o1c - o8b)" (%note-number->name note)))
  (receive (octave notenum) (quotient&remainder note 12)
    (+ (ash (- octave 1) 4) notenum)))

(define (%note-number->name note)
  (receive (octave notenum) (floor/ note 12)
    (format "o~a~a" octave
            (vector-ref #("c" "c+" "d" "d+" "e" "f" "f+" "g" "g+" "a" "a+" "b") notenum))))

(define (%opn-voice-data-get form)
  (define (operator->register opform)
    (match opform
      [(ar dr sr rr sl tl ks ml dt dt2 seg ams)
       (list (logior (ash dt 4) ml)
             tl
             (logior (ash ks 6) ar)
             (logior (ash ams 7) dr)
             sr
             (logior (ash sl 4) rr))]))
  (match form
    [((al fb) op1 op2 op3 op4)
     (sxmml-opmn-operator-zip (operator->register op1)
                              (operator->register op2)
                              (operator->register op3)
                              (operator->register op4)
                              (logior (ash fb 3) al))]))

;; CLI が設定した出力先を使い、ライブラリからの利用時だけ既定値を補う。
(define (%output-file-get)
  (or (slot-ref (sxmml-environment) 'output)
      (let1 output (or (and-let* ([current-file (sxmml-source-file)])
                         (path-swap-extension current-file "m"))
                       "./a.m")
        (slot-set! (sxmml-environment) 'output output)
        output)))

;; 出力先の指定に使われていた filename メタデータは無視し、警告を表示する
(define (%check-obsolete-metadata)
  (when (sxmml-metadata-get 'filename)
    (format (current-error-port)
            "warning: metadata 'filename' is ignored; use the -o option of sxlc to set the output file\n")))

(define (%shift_jis-u8vector-copy! u8vector str start)
  (let* ([u8str (ces-convert-to <u8vector> str "*jp" "cp932")]
         [u8len (u8vector-length u8str)])
    (u8vector-copy! u8vector start u8str)
    (+ start u8len 1)))

(define (%ticks-update! track)
  (when (> (slot-ref (sxmml-track-get track) 'total-ticks)
           (slot-ref (sxmml-pmd-binary-environment) 'max-total-ticks))
    (slot-set! (sxmml-pmd-binary-environment) 'max-total-ticks
               (slot-ref (sxmml-track-get track) 'total-ticks)))
  (when (> (slot-ref (sxmml-track-get track) 'loop-ticks)
           (slot-ref (sxmml-pmd-binary-environment) 'max-loop-ticks))
    (slot-set! (sxmml-pmd-binary-environment) 'max-loop-ticks
               (slot-ref (sxmml-track-get track) 'loop-ticks))))
