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
  (use sxmml.classes)
  (use sxmml.environment)
  (use sxmml.optimizer)
  (export sxmml-pmd-setup! sxmml-pmd-ir-optimize sxmml-pmd-voice-number-table-compute
          sxmml-pmd-metadata-value sxmml-pmd-lfo-values sxmml-pmd-envelope-values
          sxmml-pmd-check-obsolete-metadata))
(select-module sxmml.backend.pmd)

(define (sxmml-pmd-setup!)
  (define-track (A B C D E F) :volume-delta 4)
  (define-track (G H I L))
  (define-track J :volume-delta 16))

(define (sxmml-pmd-metadata-value metadata key)
  (let1 value (hash-table-get metadata key #f)
    (if (eq? key 'memo)
      (cond [(not value) ()]
            [(string? value) (list value)]
            [(and (list? value) (every string? value)) value]
            [else (errorf <sxmml-value-error> "invalid metadata value: memo is ~s (expected a string or a list of strings)" value)])
      (begin
        (unless (or (not value) (string? value))
          (errorf <sxmml-value-error> "invalid metadata value: ~a is ~s (expected a string)" key value))
        value))))

(define (sxmml-pmd-voice-number-table-compute voices)
  (let ([ids (sort (hash-table-keys voices)
                   (^[a b] (string<? (symbol->string a) (symbol->string b))))]
        [table (make-hash-table eq-comparator)])
    (when (> (length ids) 256)
      (errorf <sxmml-error> "too many voices: ~a (max 256)" (length ids)))
    (for-each (^[id number] (hash-table-put! table id number)) ids (iota (length ids)))
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
    (let1 range (assv (length value) ranges)
      (unless range
        (errorf <sxmml-value-error> "invalid ~a values: ~a has ~a value~a (expected ~a)"
                kind id (length value) (if (= (length value) 1) "" "s") expected))
      (for-each (^[v r i]
                  (unless (and (exact-integer? v) (<= (car r) v (cadr r)))
                    (errorf <sxmml-value-error> "invalid ~a ~a value ~a: ~s (expected an integer from ~a to ~a)"
                            kind id i v (car r) (cadr r))))
                value (cdr range) (iota (length value) 1))
      value)))

(define (sxmml-pmd-check-obsolete-metadata metadata)
  (when (hash-table-get metadata 'filename #f)
    (format (current-error-port)
            "warning: metadata 'filename' is ignored; use the -o option of sxlc to set the output file\n")))

(define (sxmml-pmd-ir-optimize ir level)
  (cond [(<= level 0) ir]
        [(= level 1) (sxmml-ir-optimize-local ir)]
        [(= level 2) (sxmml-ir-optimize ir)]
        [(= level 3) (sxmml-ir-optimize-with-repeats ir)]
        [else (sxmml-ir-optimize-with-repeat-expansion ir)]))
