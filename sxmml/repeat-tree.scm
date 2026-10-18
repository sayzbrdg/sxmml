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
;;; 仕様 ID local-repeat-tree/1、commit 7e521ec46950b9c9a42424ea1199db5f2170a766）をもとに、
;;; 重み付きの葉と窓幅を加えた。仕様との違いは doc/internal-reference/repeat-tree.adoc を参照
(define-module sxmml.repeat-tree
  (use gauche.record)
  (use srfi.1)
  (export make-literal literal? literal-index
          make-concat concat? concat-left concat-right
          make-repeat repeat? repeat-count repeat-body
          make-final final? final-count final-prefix final-suffix
          make-repeat-tree-settings repeat-tree-settings?
          repeat-tree-settings-repeat-costs repeat-tree-settings-final-costs
          repeat-tree-settings-allow-final? repeat-tree-settings-window
          repeat-tree-inspect
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
  (%make-repeat-tree-settings repeat-costs final-costs allow-final? window)
  repeat-tree-settings?
  (repeat-costs repeat-tree-settings-repeat-costs)
  (final-costs repeat-tree-settings-final-costs)
  (allow-final? repeat-tree-settings-allow-final?)
  (window repeat-tree-settings-window))

(define (%exact-integer? x) (and (integer? x) (exact? x)))

;; ((回数 . 制御費用) ...) を検査してコピーする。回数は昇順・重複なしの 2 以上、費用は 0 以上
(define (%copy-costs pairs)
  (let loop ([rest pairs] [previous 1] [saved ()])
    (cond [(null? rest) (reverse saved)]
          [(and (pair? rest) (pair? (car rest))
                (%exact-integer? (caar rest)) (> (caar rest) previous)
                (%exact-integer? (cdar rest)) (>= (cdar rest) 0))
           (loop (cdr rest) (caar rest) (cons (cons (caar rest) (cdar rest)) saved))]
          [else
           (errorf "repeat-tree: cost pairs require increasing exact counts >= 2 and exact fees >= 0: ~s"
                   pairs)])))

;; 使わない表も含めて全体を検査する
(define (make-repeat-tree-settings repeat-costs final-costs allow-final? window)
  (unless (boolean? allow-final?)
    (errorf "repeat-tree: the final flag must be a boolean: ~s" allow-final?))
  (unless (and (%exact-integer? window) (>= window 1))
    (errorf "repeat-tree: the window must be an exact integer >= 1: ~s" window))
  (%make-repeat-tree-settings (%copy-costs repeat-costs) (%copy-costs final-costs)
                              allow-final? window))

;;;
;;; 入力の検査
;;;
;; items と weights は同じ長さ n >= 1 のベクタで、重みは正の exact 整数
(define (%check-input items weights)
  (unless (and (vector? items) (vector? weights)
               (= (vector-length items) (vector-length weights))
               (> (vector-length items) 0))
    (error "repeat-tree: items and weights must be vectors of the same length >= 1"))
  (vector-for-each (^w (unless (and (%exact-integer? w) (> w 0))
                         (errorf "repeat-tree: weights must be exact integers > 0: ~s" w)))
                   weights))

;; 比較手続きの結果は真偽値に限る
(define (%same? same? a b)
  (rlet1 v (same? a b)
    (unless (boolean? v)
      (errorf "repeat-tree: the comparison must return a boolean: ~s" v))))

;;;
;;; 独立検査
;;;
;; 木を直接たどり、LCP・DP は使わない。訪問は 2n-1 回、部分展開の長さは n までとし、
;; 展開列は長さを確かめてから作る。通れば展開した添字のリストを返す
(define (repeat-tree-inspect items weights same? settings tree claimed-cost)
  (%check-input items weights)
  (let* ([n (vector-length items)]
         [fee (^[costs r]
                (or (and (%exact-integer? r) (>= r 2) (assv-ref costs r))
                    (errorf "repeat-tree: disallowed repeat count: ~s" r)))])
    (define (fail message) (errorf "repeat-tree: inspection failed: ~a" message))
    ;; 戻り値は (values 費用 長さ 添字列を作る手続き 残りの訪問数)
    (define (finish cost len indices remaining)
      (unless (<= 1 len n) (fail "partial expansion length limit"))
      (values cost len indices remaining))
    (define (walk node remaining)
      (when (<= remaining 0) (fail "node visit limit"))
      (let1 left (- remaining 1)
        (cond
         [(literal? node)
          (let1 index (literal-index node)
            (unless (and (%exact-integer? index) (<= 0 index) (< index n)) (fail "source index"))
            (finish (vector-ref weights index) 1 (^[] (list index)) left))]
         [(concat? node)
          (receive (ca la ia ra) (walk (concat-left node) left)
            (receive (cb lb ib rb) (walk (concat-right node) ra)
              (finish (+ ca cb) (+ la lb) (^[] (append (ia) (ib))) rb)))]
         [(repeat? node)
          (let* ([r (repeat-count node)]
                 [h (fee (repeat-tree-settings-repeat-costs settings) r)])
            (receive (ca la ia ra) (walk (repeat-body node) left)
              (finish (+ h ca) (* r la)
                      (^[] (let1 body (ia) (concatenate (make-list r body)))) ra)))]
         [(final? node)
          (unless (repeat-tree-settings-allow-final? settings) (fail "final disabled"))
          (let* ([r (final-count node)]
                 [h (fee (repeat-tree-settings-final-costs settings) r)])
            (receive (ca la ia ra) (walk (final-prefix node) left)
              (receive (cb lb ib rb) (walk (final-suffix node) ra)
                (finish (+ h ca cb) (+ (* (- r 1) (+ la lb)) la)
                        (^[] (let ([a (ia)] [b (ib)])
                               (append (concatenate (make-list (- r 1) (append a b))) a)))
                        rb))))]
         [else (fail "tree node required")])))
    (receive (cost len indices _) (walk tree (- (* 2 n) 1))
      (unless (and (%exact-integer? claimed-cost) (= claimed-cost cost))
        (fail "claimed cost mismatch"))
      (unless (= len n) (fail "expansion length mismatch"))
      (rlet1 expanded (indices)
        (for-each (^[t index] (unless (%same? same? (vector-ref items t) (vector-ref items index))
                                (fail "expanded value mismatch")))
                  (iota n) expanded)))))
