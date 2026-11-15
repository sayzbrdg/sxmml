;; IR 復元後の検査。探索の LCP・DP は参照しない。
(define-module test.optimizer-repeat-check
  (use sxmml.optimizer)
  (use sxmml.native)
  (use srfi.1)
  (export check-repeat-ir))
(select-module test.optimizer-repeat-check)
(define %repeat-block? (with-module sxmml.optimizer %repeat-block?))
(define %repeat-block-start (with-module sxmml.optimizer %repeat-block-start))
(define %repeat-block-front (with-module sxmml.optimizer %repeat-block-front))
(define %repeat-block-back (with-module sxmml.optimizer %repeat-block-back))
(define %repeat-block-break (with-module sxmml.optimizer %repeat-block-break))
(define %repeat-block-end (with-module sxmml.optimizer %repeat-block-end))
(define %repeat-structure (with-module sxmml.optimizer %repeat-structure))

;; 圧縮の出力 output が、圧縮の入力 input（要素のリスト）と同じ列を表すかを確かめる。
;; 生成したループは一つの出現を繰り返し使うので、固定 IR 比較で比べる。入れ子の深い
;; 既存のリピートは展開すると大きくなるので、生成したループ（[ が入力にないもの）だけを
;; 展開し、既存のリピートは回数・: の有無・本文で比べる
(define (check-repeat-ir output input)
  (define same? (make-sxmml-native-ir-comparison))
  (define originals
    (rlet1 h (make-hash-table eq-comparator)
      (let mark ([items input])
        (dolist [i items]
          (when (%repeat-block? i)
            (hash-table-put! h (%repeat-block-start i) #t)
            (mark (%repeat-block-front i))
            (mark (or (%repeat-block-back i) ())))))))
  (define (unfold items tail)
    (fold-right
     (^[i tail]
       (if (and (%repeat-block? i)
                (not (hash-table-contains? originals (%repeat-block-start i))))
         (let ([count (slot-ref (%repeat-block-end i) 'value)]
               [front (unfold (%repeat-block-front i) ())]
               [back (unfold (or (%repeat-block-back i) ()) ())])
           (let loop ([remaining (- count 1)] [result (fold-right cons tail front)])
             (if (zero? remaining)
               result
               (loop (- remaining 1) (fold-right cons (fold-right cons result back) front)))))
         (cons i tail)))
     tail items))
  (define (match? xs ys)
    (cond [(null? xs) (null? ys)]
          [(null? ys) #f]
          [else
           (let ([x (car xs)] [y (car ys)])
             (and (cond [(and (%repeat-block? x) (%repeat-block? y))
                         (and (equal? (slot-ref (%repeat-block-end x) 'value)
                                      (slot-ref (%repeat-block-end y) 'value))
                              (eq? (not (%repeat-block-break x)) (not (%repeat-block-break y)))
                              (match? (unfold (%repeat-block-front x) ()) (%repeat-block-front y))
                              (match? (unfold (or (%repeat-block-back x) ()) ()) (or (%repeat-block-back y) ())))]
                        [(or (%repeat-block? x) (%repeat-block? y)) #f]
                        [else (same? x y)])
                  (match? (cdr xs) (cdr ys))))]))
  (unless (match? (unfold (%repeat-structure output) ()) input)
    (error "repeat optimization changed the expanded objects")))
