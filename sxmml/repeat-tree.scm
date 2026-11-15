;;; Copyright (c) 2026 Seiji Ohashi <sayzbrdg@gmail.com>
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

;;; 局所反復木による列の圧縮。repeat-tree の Gauche 移植（実装 ID gauche-all-periods/2、
;;; commit 7e521ec46950b9c9a42424ea1199db5f2170a766）をもとに、重み付きの葉と窓幅を加えた。
;;; 圧縮方式と成立条件は doc/internal-reference/repeat-tree.adoc を参照
(define-module sxmml.repeat-tree
  (use gauche.record)
  (use sxmml.native)
  (use gauche.sequence)
  (use srfi.1)
  (export make-literal literal? literal-index
          make-concat concat? concat-left concat-right
          make-repeat repeat? repeat-count repeat-body
          make-final final? final-count final-prefix final-suffix
          make-repeat-tree-settings repeat-tree-settings?
          repeat-tree-settings-repeat-costs repeat-tree-settings-final-costs
          repeat-tree-settings-allow-final? repeat-tree-settings-window
          repeat-tree-compress
          ))
(select-module sxmml.repeat-tree)

;;;
;;; 木
;;;
;; フィールドは変更不可。子や添字の妥当性は検査で確かめる
(define-record-type <literal> (make-literal index) literal?
  (index literal-index))
(define-record-type <concat> (make-concat left right) concat?
  (left concat-left) (right concat-right))
(define-record-type <repeat> (make-repeat count body) repeat?
  (count repeat-count) (body repeat-body))
;; (prefix suffix)^(count-1) prefix を表す
(define-record-type <final> (make-final count prefix suffix) final?
  (count final-count) (prefix final-prefix) (suffix final-suffix))

;;;
;;; 設定
;;;
(define-record-type <repeat-tree-settings>
  (%make-repeat-tree-settings repeat-costs final-costs allow-final window)
  repeat-tree-settings?
  (repeat-costs repeat-tree-settings-repeat-costs)
  (final-costs repeat-tree-settings-final-costs)
  (allow-final repeat-tree-settings-allow-final?)
  (window repeat-tree-settings-window))

;; ((回数 . 制御費用) ...) を検査してコピーする。回数は昇順・重複なしの 2 以上、費用は 0 以上
(define (%copy-costs pairs)
  (let loop ([rest pairs] [previous 1] [saved ()])
    (cond [(null? rest) (reverse! saved)]
          [(and (pair? rest) (pair? (car rest))
                (exact-integer? (caar rest)) (> (caar rest) previous)
                (exact-integer? (cdar rest)) (>= (cdar rest) 0))
           (loop (cdr rest) (caar rest) (cons (cons (caar rest) (cdar rest)) saved))]
          [else
           (errorf "repeat-tree: cost pairs require increasing exact counts >= 2 and exact fees >= 0: ~s"
                   pairs)])))

;; 使わない表も含めて全体を検査する
(define (make-repeat-tree-settings repeat-costs final-costs allow-final? window)
  (unless (boolean? allow-final?)
    (errorf "repeat-tree: the final flag must be a boolean: ~s" allow-final?))
  (unless (and (exact-integer? window) (>= window 1))
    (errorf "repeat-tree: the window must be an exact integer >= 1: ~s" window))
  (%make-repeat-tree-settings (%copy-costs repeat-costs) (%copy-costs final-costs)
                              allow-final? window))

;;;
;;; 入力の検査
;;;
;; items と weights は同じ長さ n >= 1 のベクタで、重みは正の exact 整数
(define (%check-input items weights settings)
  (unless (repeat-tree-settings? settings)
    (errorf "repeat-tree: settings required: ~s" settings))
  (unless (and (vector? items) (vector? weights)
               (= (vector-length items) (vector-length weights))
               (> (vector-length items) 0))
    (error "repeat-tree: items and weights must be vectors of the same length >= 1"))
  (vector-for-each (^w (unless (and (exact-integer? w) (> w 0))
                         (errorf "repeat-tree: weights must be exact integers > 0: ~s" w)))
                   weights))

(define (repeat-tree-compress items weights same? settings)
  (%check-input items weights settings)
  (sxmml-native-repeat-tree-compress items weights same? settings))
