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
  (use srfi.1)
  (use util.match)
  (use sxmml.voice)
  (use gauche.sequence)
  (use sxmml.classes)
  (use sxmml.environment)
  (use sxmml.optimizer)
  (export sxmml-pmd-for-each-track-ir sxmml-pmd-output-tracks-select sxmml-pmd-pcm-voice-number sxmml-pmd-setup! sxmml-pmd-ir-optimize sxmml-pmd-voice-number-table-compute
          sxmml-pmd-metadata-value sxmml-pmd-lfo-values sxmml-pmd-envelope-values
          sxmml-pmd-check-obsolete-metadata))
(select-module sxmml.backend.pmd)

(define %pmd-parts
  '((A opn 4) (B opn 4) (C opn 4) (D opn 4) (E opn 4) (F opn 4)
    (G ssg 1) (H ssg 1) (I ssg 1) (J pcm 16) (L rhythm 1)))

(define (sxmml-pmd-setup!)
  (for-each (match-lambda
              [(part source delta)
               (sxmml-track-set! part :part part :sound-source source :volume-delta delta)])
            %pmd-parts))

(define (sxmml-pmd-output-tracks-select env :optional (backend 'pmd-source))
  (define tracks (make-hash-table eq-comparator))
  (define (select part source)
    (let* ([explicit (hash-table-find tracks (^[name track] (and (eq? part (slot-ref track 'part)) track)))]
           [named (hash-table-get tracks part #f)]
           [implicit (and named (not (slot-ref named 'part)) named)]
           [track (or explicit implicit)])
      (when (and explicit implicit)
        (errorf <sxmml-value-error> "conflicting PMD part ~s: track ~s (implicit) and track ~s (explicit)"
                part part (slot-ref explicit 'name)))
      (when track
        (let1 requested (slot-ref track 'sound-source)
          (when (and requested (not (eq? requested source)))
            (errorf <sxmml-value-error> "invalid track ~s sound-source: ~s (expected ~s for part ~s)"
                    (slot-ref track 'name) requested source part))))
      track))
  (hash-table-for-each (slot-ref env 'track)
    (^[name track]
      (let1 kind (slot-ref track 'sound-source)
        (if (or (not kind) (memq kind '(opn ssg pcm rhythm)))
          (hash-table-put! tracks name track)
          (format (current-error-port) "warning: ~a: ignoring track ~a (unsupported sound-source: ~s)~%" backend name kind)))))
  (map (match-lambda [(part source _) (select part source)]) %pmd-parts))

(define (sxmml-pmd-pcm-voice-number env name)
  (define (sample n)
    (unless (and (exact-integer? n) (<= 0 n 255))
      (errorf <sxmml-value-error> "invalid PCM voice number: ~s (expected an integer from 0 to 255)" n))
    n)
  (if-let1 voice (and-let* ([table (sxmml-voice-table-find env 'pcm)]) (hash-table-get table name #f))
    (match (slot-ref voice 'value)
      [(0 n) (sample n)]
      [((? string? file) n)
       (unless (equal? file (sxmml-pmd-metadata-value (slot-ref env 'metadata) 'pcmfile))
         (errorf <sxmml-value-error> "unknown PCM file: ~s" file))
       (sample n)]
      [(? number? n) (sample n)]
      [other (errorf <sxmml-value-error> "invalid PCM definition ~a: ~s" name other)])
    (if-let1 n (string->number (symbol->string name))
      (sample n)
      (errorf <sxmml-reference-error> "undefined PCM voice: ~a" name))))

(define (sxmml-pmd-metadata-value metadata key)
  (let1 value (hash-table-get metadata key #f)
    (cond [(and value (memq key '(pcmfile ppzfile)) (not (string? value)))
           (let1 limit (if (eq? key 'pcmfile) 1 2)
             (unless (and (list? value) (every string? value) (<= (length value) limit))
               (errorf <sxmml-value-error> "invalid metadata value: ~a is ~s (expected a string or a list of up to ~a strings)" key value limit))
             (string-join value ","))]
          [(eq? key 'memo)
      (cond [(not value) ()]
            [(string? value) (list value)]
            [(and (list? value) (every string? value)) value]
            [else (errorf <sxmml-value-error> "invalid metadata value: memo is ~s (expected a string or a list of strings)" value)])]
      [else
        (unless (or (not value) (string? value))
          (errorf <sxmml-value-error> "invalid metadata value: ~a is ~s (expected a string)" key value))
        value])))

(define (sxmml-pmd-voice-number-table-compute voices)
  (let* ([ids (sort (hash-table-keys voices)
                    (^[a b] (string<? (symbol->string a) (symbol->string b))))]
         [count (length ids)]
         [table (make-hash-table eq-comparator)])
    (when (> count 256)
      (errorf <sxmml-error> "too many voices: ~a (max 256)" count))
    (for-each-with-index (^[number id] (hash-table-put! table id number)) ids)
    table))

(define %pmd-lfo-types '(default triangle))

(define %pmd-lfo-ranges
  '((4 . ((0 255) (0 255) (-128 127) (0 255)))))

(define %pmd-envelope-types '(default))

(define %pmd-envelope-ranges
  '((4 . ((0 255) (-15 15) (0 255) (0 255)))
    (5 . ((0 31) (0 31) (0 31) (0 15) (0 15)))
    (6 . ((0 31) (0 31) (0 31) (0 15) (0 15) (0 15)))))

(define (sxmml-pmd-lfo-values lfo)
  (%pmd-definition-values "LFO" lfo %pmd-lfo-types %pmd-lfo-ranges "4"))

(define (sxmml-pmd-envelope-values envelope)
  (%pmd-definition-values "envelope" envelope %pmd-envelope-types %pmd-envelope-ranges "4, 5 or 6"))

(define (%pmd-definition-values kind obj types ranges expected)
  (let ([id (slot-ref obj 'id)]
        [type (slot-ref obj 'type)]
        [value (slot-ref obj 'value)])
    (unless (memq type types)
      (errorf <sxmml-value-error> "unsupported ~a type: ~a in ~a (supported: ~a)"
              kind type id (string-join (map symbol->string types) ", ")))
    (let* ([count (length value)] [range (assv count ranges)])
      (unless range
        (errorf <sxmml-value-error> "invalid ~a values: ~a has ~a value~a (expected ~a)"
                kind id count (if (= count 1) "" "s") expected))
      (for-each-with-index (^[i v r]
                  (unless (and (exact-integer? v) (<= (car r) v (cadr r)))
                    (errorf <sxmml-value-error> "invalid ~a ~a value ~a: ~s (expected an integer from ~a to ~a)"
                            kind id (+ i 1) v (car r) (cadr r))))
                value (cdr range))
      value)))

(define (sxmml-pmd-check-obsolete-metadata metadata)
  (when (hash-table-get metadata 'filename #f)
    (format (current-error-port)
            "warning: metadata 'filename' is ignored; use the -o option of sxlc to set the output file\n")))

(define (sxmml-pmd-for-each-track-ir emit tracks env)
  (define level (slot-ref env 'optimize))
  (define jobs (slot-ref env 'jobs))
  (define (ir track) (if track (slot-ref track 'ir) '()))
  (define (output i spec track commands)
    (emit i (car spec) (cadr spec) track commands))
  (define (task track)
    (and (pair? (ir track))
         (cut sxmml-pmd-ir-optimize (ir track) level (slot-ref track 'name))))
  (cond [(= level 9)
         (for-each output (iota (length tracks)) %pmd-parts tracks
                   (sxmml-run-compression-jobs (map task tracks) jobs))]
        [(and (>= level 3) (> jobs 1) (> (count (^[t] (pair? (ir t))) tracks) 1))
         (let1 compressed (sxmml-ir-compress-repeats-batch
                           (map (^[t] (sxmml-ir-optimize (ir t))) tracks)
                           :jobs jobs :expand-small (> level 3))
           (for-each output (iota (length tracks)) %pmd-parts tracks compressed))]
        [else
         (for-each (^[i spec t] (output i spec t (if t (sxmml-pmd-ir-optimize (ir t) level) '())))
                   (iota (length tracks)) %pmd-parts tracks)]))

(define (sxmml-pmd-ir-optimize ir level :optional (track-name #f))
  (define (warn count)
    (format (current-error-port)
            "warning: ~afull-width repeat search over ~a elements may take a long time. Press Ctrl-C to cancel.\n"
            (if track-name (format #f "track ~a: " track-name) "") count))
  (cond [(<= level 0) ir]
        [(= level 9) (sxmml-ir-optimize-full-repeat ir warn)]
        [(= level 1) (sxmml-ir-optimize-local ir)]
        [(= level 2) (sxmml-ir-optimize ir)]
        [(= level 3) (sxmml-ir-optimize-with-repeats ir)]
        [else (sxmml-ir-optimize-with-repeat-expansion ir)]))
