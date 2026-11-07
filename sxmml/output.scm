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

;;; 登録環境から出力に必要な情報を準備する。
(define-module sxmml.output
  (use sxmml.classes)
  (export make-sxmml-output-environment sxmml-ir-flatten sxmml-output-track-find))
(select-module sxmml.output)

(define (sxmml-ir-flatten ir-tree)
  (reverse!
   (let recur ([rest ir-tree] [result ()])
     (cond [(null? rest) result]
           [(pair? (car rest)) (recur (cdr rest) (recur (car rest) result))]
           [(or (null? (car rest)) (is-a? (car rest) <sxmml-mml-nop>))
            (recur (cdr rest) result)]
           [else (recur (cdr rest) (cons (car rest) result))]))))

(define (make-sxmml-output-environment registered-environment
                                      :key (output (undefined)) (optimize (undefined)) (source #f))
  (when (undefined? output)
    (error <sxmml-value-error> "make-sxmml-output-environment requires :output"))
  (when (undefined? optimize)
    (error <sxmml-value-error> "make-sxmml-output-environment requires :optimize"))
  (rlet1 prepared
      (make <sxmml-output-environment>
        :resolution (slot-ref registered-environment 'resolution)
        :metadata (slot-ref registered-environment 'metadata)
        :voice (slot-ref registered-environment 'voice)
        :envelope (slot-ref registered-environment 'envelope)
        :lfo (slot-ref registered-environment 'lfo)
        :output output :optimize optimize :source source)
    (hash-table-for-each
     (slot-ref registered-environment 'track)
     (^[name context]
       (let1 ticks (car (slot-ref context 'current-ticks))
         (hash-table-set!
          (slot-ref prepared 'track) name
          (make <sxmml-output-track>
            :name (slot-ref context 'track) :part (slot-ref context 'part)
            :ir (sxmml-ir-flatten (slot-ref context 'compiled-data))
            :total-ticks (car ticks) :loop-ticks (- (car ticks) (cdr ticks))
            :loop-exists (slot-ref context 'loop-exists)
            :ticks-statistics (slot-ref context 'ticks-statistics))))))))

(define (sxmml-output-track-find output-environment part)
  (hash-table-find (slot-ref output-environment 'track)
                   (^[name track] (and (eq? (slot-ref track 'part) part) track))))
