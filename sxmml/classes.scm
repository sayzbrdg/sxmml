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

(define-module sxmml.classes
  (export-all))
(select-module sxmml.classes)


;;;
;;; Environment
;;;
(define-class <sxmml-environment> ()
  ([resolution :init-keyword :resolution :init-value 24]
   [metadata :init-form (make-hash-table eq-comparator)]
   [lfo :init-form (make-hash-table eq-comparator)]
   [envelope :init-form (make-hash-table eq-comparator)]
   [voice :init-form (make-hash-table eq-comparator)]
   [track :init-form (make-hash-table eq-comparator)]
   [optimize :init-keyword :optimize :init-value 1]
   [mode :init-keyword :mode :init-value ""]
   [backend :init-keyword :backend :init-value #f]
   ))

;;;
;;; Optimizer
;;;
(define-class <sxmml-optimize-environment> ()
  ([inter-objects :init-form (make-hash-table eq-comparator)]
   [objects-order :init-form (make-tree-map integer-comparator)]
   [current-ticks :init-form (acons 0 0 ())]
   ))

(define-class <sxmml-optimize-2path-context> ()
  ([voice :init-value #f]
   [voice-placed? :init-value #f]
   [volume :init-value #f]
   [volume-placed? :init-value #f]
   [gatetime :init-value #f]
   [gatetime-placed? :init-value #f]
   [pan :init-value #f]
   [pan-placed? :init-value #f]
   [detune :init-value #f]
   [detune-placed? :init-value #f]
   [envelope :init-value #f]
   [envelope-placed? :init-value #f]
   [lfo :init-value #f]
   [lfo-placed? :init-value #f]
   [lfo-switch :init-value #f]
   [lfo-switch-placed? :init-value #f]
   [repeat-depth :init-value 0]))

;;;
;;; MML classes
;;;
(define-class <sxmml-mml-context> ()
  ([track :init-value #f :init-keyword :track]
   [part :init-value #f :init-keyword :part]
   [default-length :init-value 4 :init-keyword :default-length]
   [volume-delta :init-value 1 :init-keyword :volume-delta]
   [current-octave :init-value 4 :init-keyword :current-octave]
   [current-transpose :init-value 0 :init-keyword :current-transpose]
   [total-ticks :init-value 0 :init-keyword :total-ticks]
   [loop-ticks :init-value 0 :init-keyword :loop-ticks]
   [compiled-data :init-value ()]
   [repeat-stack :init-value ()]
   ))

(define-class <sxmml-mml> ()
  ([value :init-keyword :value]))
(define-class <sxmml-mml-nop> (<sxmml-mml>) ())

;; note & rest
(define-class <sxmml-mml-note> (<sxmml-mml>)
  ([ticks :init-value #f :init-keyword :ticks]))
(define-class <sxmml-mml-rest> (<sxmml-mml-note>) ())

;; commands
(define-class <sxmml-mml-length> (<sxmml-mml>) ())
(define-class <sxmml-mml-octave> (<sxmml-mml>) ())
(define-class <sxmml-mml-volume> (<sxmml-mml>) ())
(define-class <sxmml-mml-volume-change> (<sxmml-mml>) ())
(define-class <sxmml-mml-gatetime> (<sxmml-mml>) ())
(define-class <sxmml-mml-tie> (<sxmml-mml>) ())
(define-class <sxmml-mml-transpose> (<sxmml-mml>) ())
(define-class <sxmml-mml-transpose-change> (<sxmml-mml>) ())
(define-class <sxmml-mml-tempo> (<sxmml-mml>) ())
(define-class <sxmml-mml-tempo-absolute> (<sxmml-mml>) ())
(define-class <sxmml-mml-detune> (<sxmml-mml>) ())
(define-class <sxmml-mml-loop> (<sxmml-mml>) ())
(define-class <sxmml-mml-repeat-start> (<sxmml-mml>) ())
(define-class <sxmml-mml-repeat-break> (<sxmml-mml>) ())
(define-class <sxmml-mml-repeat-end> (<sxmml-mml>) ())

(define-class <sxmml-mml-voice> (<sxmml-mml>) ())
(define-class <sxmml-mml-pan> (<sxmml-mml>) ())
(define-class <sxmml-mml-lfo> (<sxmml-mml>)
  ([channel :init-keyword :channel :init-value #f]))
(define-class <sxmml-mml-lfo-switch> (<sxmml-mml-lfo>) ())
(define-class <sxmml-mml-envelope> (<sxmml-mml>) ())
(define-class <sxmml-mml-portament> (<sxmml-mml-note>) ())
(define-class <sxmml-mml-noise-frequency> (<sxmml-mml>) ())
(define-class <sxmml-mml-output-switch> (<sxmml-mml>) ())

;; rhythm
(define-class <sxmml-rhythm> ()
  ([value :init-keyword :value :init-value #f]
   [bass :init-keyword :bass :init-value #f]
   [snare :init-keyword :snare :init-value #f]
   [tom :init-keyword :tom :init-value #f]
   [cymbal :init-keyword :cymbal :init-value #f]
   [hihat :init-keyword :hihat :init-value #f]
   [rimshot :init-keyword :rimshot :init-value #f]))
(define-class <sxmml-rhythm-emit> (<sxmml-rhythm> <sxmml-mml-note>) ())
(define-class <sxmml-rhythm-volume> (<sxmml-rhythm>) ())
(define-class <sxmml-rhythm-volume-change> (<sxmml-rhythm>) ())
(define-class <sxmml-rhythm-pan> (<sxmml-rhythm>) ())

;; LFO
(define-class <sxmml-lfo> ()
  ([id :init-keyword :id]
   [type :init-keyword :type]
   [value :init-keyword :value]))

;; envelope
(define-class <sxmml-envelope> ()
  ([id :init-keyword :id]
   [type :init-keyword :type]
   [value :init-keyword :value]))

;; voice
(define-class <sxmml-voice> ()
  ([id :init-keyword :id]
   [number :init-keyword :number]
   [value :init-keyword :value]))

(define-class <sxmml-opmn-voice> (<sxmml-voice>) ())
(define-class <sxmml-opm-voice> (<sxmml-opmn-voice>) ())
(define-class <sxmml-opn-voice> (<sxmml-opmn-voice>) ())
