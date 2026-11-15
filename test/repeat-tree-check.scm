;; 圧縮結果を探索と独立に検査するテスト支援。
(define-module test.repeat-tree-check
  (use gauche.sequence)
  (use srfi.1)
  (use sxmml.repeat-tree)
  (export inspect-repeat-tree))
(select-module test.repeat-tree-check)
(define %check-input (with-module sxmml.repeat-tree %check-input))

;; 比較手続きの結果は真偽値に限る
(define (%same? same? a b)
  (rlet1 v (same? a b)
    (unless (boolean? v)
      (errorf "repeat-tree: the comparison must return a boolean: ~s" v))))

;;;
;;; 独立検査
;;;
;; 木を直接たどり、LCP・DP は使わない。訪問は 2n-1 回、部分展開の長さは n までとし、
;; 展開列は長さを確かめてから作る。通れば展開した添字のリストを返す。添字列は、後に続く
;; 列を受け取って前に足す手続きで作り、全体で O(n) にする
(define (inspect-repeat-tree items weights same? settings tree claimed-cost)
  (%check-input items weights settings)
  (let* ([n (vector-length items)]
         [fee (^[costs r]
                (or (and (exact-integer? r) (>= r 2) (assv-ref costs r))
                    (errorf "repeat-tree: disallowed repeat count: ~s" r)))])
    (define (fail message) (errorf "repeat-tree: inspection failed: ~a" message))
    ;; 戻り値は (values 費用 長さ 添字列を tail の前に足す手続き 残りの訪問数)
    (define (finish cost len indices remaining)
      (unless (<= 1 len n) (fail "partial expansion length limit"))
      (values cost len indices remaining))
    (define (walk node remaining)
      (when (<= remaining 0) (fail "node visit limit"))
      (let1 left (- remaining 1)
        (cond
         [(literal? node)
          (let1 index (literal-index node)
            (unless (and (exact-integer? index) (<= 0 index) (< index n)) (fail "source index"))
            (finish (vector-ref weights index) 1 (cut cons index <>) left))]
         [(concat? node)
          (receive (ca la ia ra) (walk (concat-left node) left)
            (receive (cb lb ib rb) (walk (concat-right node) ra)
              (finish (+ ca cb) (+ la lb) (^[tail] (ia (ib tail))) rb)))]
         [(repeat? node)
          (let* ([r (repeat-count node)]
                 [h (fee (repeat-tree-settings-repeat-costs settings) r)])
            (receive (ca la ia ra) (walk (repeat-body node) left)
              (finish (+ h ca) (* r la)
                      (^[tail] (let loop ([k r] [acc tail])
                                 (if (zero? k) acc (loop (- k 1) (ia acc)))))
                      ra)))]
         [(final? node)
          (unless (repeat-tree-settings-allow-final? settings) (fail "final disabled"))
          (let* ([r (final-count node)]
                 [h (fee (repeat-tree-settings-final-costs settings) r)])
            (receive (ca la ia ra) (walk (final-prefix node) left)
              (receive (cb lb ib rb) (walk (final-suffix node) ra)
                (finish (+ h ca cb) (+ (* (- r 1) (+ la lb)) la)
                        (^[tail] (let loop ([k (- r 1)] [acc (ia tail)])
                                   (if (zero? k) acc (loop (- k 1) (ia (ib acc))))))
                        rb))))]
         [else (fail "tree node required")])))
    (receive (cost len indices _) (walk tree (- (* 2 n) 1))
      (unless (and (exact-integer? claimed-cost) (= claimed-cost cost))
        (fail "claimed cost mismatch"))
      (unless (= len n) (fail "expansion length mismatch"))
      (rlet1 expanded (indices ())
        (for-each-with-index (^[t index] (unless (%same? same? (vector-ref items t) (vector-ref items index))
                                (fail "expanded value mismatch")))
                  expanded)))))
