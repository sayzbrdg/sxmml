;;;
;;; Test sxmml.repeat-tree
;;;

(use gauche.test)

(test-start "sxmml.repeat-tree")
(use sxmml.repeat-tree)
(test-module 'sxmml.repeat-tree)

;; 木を比べやすい S 式にする
(define (tree->sexp t)
  (cond [(literal? t) `(lit ,(literal-index t))]
        [(concat? t) `(cat ,(tree->sexp (concat-left t)) ,(tree->sexp (concat-right t)))]
        [(repeat? t) `(rep ,(repeat-count t) ,(tree->sexp (repeat-body t)))]
        [(final? t) `(fin ,(final-count t) ,(tree->sexp (final-prefix t)) ,(tree->sexp (final-suffix t)))]))
(define (make-unit-weights n) (make-vector n 1))
(define s-final3 (make-repeat-tree-settings '() '((3 . 0)) #t 256))
(define s-repeat2 (make-repeat-tree-settings '((2 . 2)) '() #f 256))

;; 設定の検査
(test* "settings accept valid tables" #t
       (repeat-tree-settings? (make-repeat-tree-settings '((2 . 2)) '((2 . 3) (255 . 3)) #t 1)))
(dolist [c `(("a count less than 2" ((1 . 0)) () #t 1)
             ("counts not increasing" ((3 . 0) (2 . 0)) () #t 1)
             ("a duplicated count" ((2 . 0) (2 . 1)) () #t 1)
             ("a negative fee" () ((2 . -1)) #t 1)
             ("an inexact fee" ((2 . 1.5)) () #t 1)
             ("a non-boolean final flag" () () yes 1)
             ("a window 0" () () #t 0)
             ("an inexact window" () () #t 1.5))]
  (test* #"settings reject ~(car c)" (test-error <error>)
         (apply make-repeat-tree-settings (cdr c))))

;; 独立検査
(test* "inspect accepts a final tree" '(0 1 0 1 0)
       (repeat-tree-inspect #(0 1 0 1 0) (make-unit-weights 5) eqv? s-final3
                            (make-final 3 (make-literal 0) (make-literal 1)) 2))
(test* "inspect counts weights" '(0 0)
       (repeat-tree-inspect #(a a) #(5 5) eq? s-repeat2 (make-repeat 2 (make-literal 0)) 7))
(dolist [c `(("a disallowed count" #(a a) #(5 5) ,s-repeat2 ,(make-repeat 3 (make-literal 0)) 7)
             ("a wrong cost" #(0 1 0 1 0) ,(make-unit-weights 5) ,s-final3
              ,(make-final 3 (make-literal 0) (make-literal 1)) 3)
             ("a wrong expansion" #(0 1) ,(make-unit-weights 2) ,s-repeat2 ,(make-repeat 2 (make-literal 0)) 3)
             ("an index out of range" #(0) ,(make-unit-weights 1) ,s-repeat2 ,(make-literal 1) 1)
             ("a disabled final" #(0 1 0 1 0) ,(make-unit-weights 5) ,(make-repeat-tree-settings '() '((3 . 0)) #f 256)
              ,(make-final 3 (make-literal 0) (make-literal 1)) 2)
             ;; 回数は許可されているが、展開列を作る前に長さで失敗する
             ("a too long expansion" #(0 0) ,(make-unit-weights 2) ,(make-repeat-tree-settings '((255 . 0)) '() #f 256)
              ,(make-repeat 255 (make-literal 0)) 1)
             ("a non-tree value" #(0) ,(make-unit-weights 1) ,s-repeat2 (lit 0) 1))]
  (test* #"inspect rejects ~(car c)" (test-error <error>)
         (repeat-tree-inspect (list-ref c 1) (list-ref c 2) eqv? (list-ref c 3) (list-ref c 4) (list-ref c 5))))
;; 設定でない値は、圧縮と同じ repeat-tree: のメッセージで失敗する
(test* "inspect rejects a non-settings value with the repeat-tree message" #t
       (let1 message (guard (e [else (condition-message e)])
                       (repeat-tree-inspect #(0) (make-unit-weights 1) eqv? 'settings (make-literal 0) 1)
                       'no-error)
         (boolean (and (string? message) (#/^repeat-tree: settings required/ message)))))

;; 圧縮。仕様 §4.1 の ABABA の例
(define (compress-sexp items weights rc fc final? :optional (window 256))
  (receive (cost tree)
      (repeat-tree-compress items weights eqv? (make-repeat-tree-settings rc fc final? window))
    (list cost (tree->sexp tree))))
(test* "ABABA with final 3 fee 0" '(2 (fin 3 (lit 0) (lit 1)))
       (compress-sexp #(0 1 0 1 0) (make-unit-weights 5) '() '((3 . 0)) #t))
(test* "ABABA with final 3 fee 2" '(4 (fin 3 (lit 0) (lit 1)))
       (compress-sexp #(0 1 0 1 0) (make-unit-weights 5) '() '((3 . 2)) #t))
(test* "ABABA with final 3 fee 3 keeps raw"
       '(5 (cat (cat (cat (cat (lit 0) (lit 1)) (lit 2)) (lit 3)) (lit 4)))
       (compress-sexp #(0 1 0 1 0) (make-unit-weights 5) '() '((3 . 3)) #t))
(test* "ABABA with final disabled" 5
       (car (compress-sexp #(0 1 0 1 0) (make-unit-weights 5) '() '((3 . 0)) #f)))
(test* "weighted leaves" '(7 (rep 2 (lit 0)))
       (compress-sexp #(a a) #(5 5) '((2 . 2)) '() #f))
;; 同じ費用 5 の候補のうち、分割点 k=1 が、k=6 の分割や Final より先に選ばれる
(test* "concat wins ties over loops" '(5 (cat (lit 0) (rep 3 (cat (lit 1) (lit 2)))))
       (compress-sexp #(0 1 0 1 0 1 0) (make-unit-weights 7) '((3 . 2)) '((4 . 3)) #t))

;; 展開の一致を直接確かめる、独立した最小費用（試験専用）
(define (oracle items weights rc fc final?)
  (let* ([n (vector-length items)] [memo (make-hash-table equal-comparator)])
    (define (same-run? i len j) ; [i,i+len) と [j,j+len) が eqv?
      (every (^k (eqv? (vector-ref items (+ i k)) (vector-ref items (+ j k)))) (iota len)))
    (define (best i j)
      (or (hash-table-get memo (cons i j) #f)
          (rlet1 v (let1 m (- j i)
                     (apply min
                            (fold + 0 (map (cut vector-ref weights <>) (iota m i)))
                            (append
                             (map (^k (+ (best i k) (best k j))) (iota (- m 1) (+ i 1)))
                             (append-map
                              (^[p] (append
                                     (filter-map (^[rf] (and (= m (* (car rf) p))
                                                             (every (^c (same-run? i p (+ i (* c p))))
                                                                    (iota (- (car rf) 1) 1))
                                                             (+ (best i (+ i p)) (cdr rf))))
                                                 rc)
                                     (if final?
                                       (append-map
                                        (^[a] (filter-map (^[rf] (and (= m (+ (* (- (car rf) 1) p) a))
                                                                      (every (^c (same-run? i p (+ i (* c p))))
                                                                             (iota (- (car rf) 2) 1))
                                                                      (same-run? i a (+ i (* (- (car rf) 1) p)))
                                                                      (+ (best i (+ i a)) (best (+ i a) (+ i p))
                                                                         (cdr rf))))
                                                          fc))
                                        (iota (- p 1) 1))
                                       '())))
                              (iota (- m 1) 1)))))
            (hash-table-put! memo (cons i j) v))))
    (best 0 n)))
;; alphabet 種類の記号からなる長さ 1～max-length の全列
(define (all-sequences alphabet max-length)
  (append-map (^[len] (let loop ([k len])
                        (if (zero? k)
                          '(())
                          (append-map (^s (map (cut cons <> s) (iota alphabet))) (loop (- k 1))))))
              (iota max-length 1)))
(define oracle-settings
  '((((2 . 2) (3 . 2) (4 . 2)) ((2 . 3) (3 . 3)) #t)
    (((3 . 0)) () #f)
    (() ((2 . 0) (4 . 1)) #t)
    (() () #t)))
(define oracle-weights
  (list make-unit-weights (^n (list->vector (map (^i (+ (modulo i 3) 1)) (iota n))))))
(test* "exact cost equals the oracle" '()
       (append-map
        (^[seq] (let* ([items (list->vector seq)] [n (vector-length items)])
                  (append-map
                   (^[s] (filter-map
                          (^[wf] (let* ([weights (wf n)]
                                        [expected (apply oracle items weights s)]
                                        [actual (car (apply compress-sexp items weights s))])
                                   (and (not (= expected actual)) (list seq s weights expected actual))))
                          oracle-weights))
                   oracle-settings)))
        (append (all-sequences 2 7) (all-sequences 3 5))))

;; 窓幅。長さ w 以下のブロックの連結に限った最小費用（試験専用）
(define (window-oracle items weights rc fc final? w)
  (let* ([n (vector-length items)] [g (make-vector (+ n 1) #f)])
    (vector-set! g 0 0)
    (dolist [j (iota n 1)]
      (vector-set! g j (apply min (map (^i (+ (vector-ref g i)
                                              (oracle (vector-copy items i j) (vector-copy weights i j)
                                                      rc fc final?)))
                                       (iota (- j (max 0 (- j w))) (max 0 (- j w)))))))
    (vector-ref g n)))
(test* "windowed cost equals the window oracle" '()
       (append-map
        (^[seq] (let* ([items (list->vector seq)] [n (vector-length items)])
                  (append-map
                   (^[s] (append-map
                          (^[wf] (filter-map
                                  (^[w] (let* ([weights (wf n)]
                                               [expected (apply window-oracle items weights `(,@s ,w))]
                                               [actual (car (apply compress-sexp items weights `(,@s ,w)))])
                                          (and (not (= expected actual)) (list seq s weights w expected actual))))
                                  '(1 2 3)))
                          oracle-weights))
                   (list (list-ref oracle-settings 0) (list-ref oracle-settings 2)))))
        (all-sequences 2 7)))
(test* "window 1 keeps raw" 4
       (car (compress-sexp #(0 0 0 0) (make-unit-weights 4) '((4 . 0)) '() #f 1)))
;; ABABA CCCC は、k=5 の分割が最初に費用 5 に達し、Final と Repeat の連結になる。
;; W >= n では全体の連結の始点 0 が選ばれ、厳密な区間 DP の木と同じになる
(dolist [w '(9 12)]
  (test* #"a window ~w not smaller than n gives the exact tree"
         '(5 (cat (fin 3 (lit 0) (lit 1)) (rep 4 (lit 5))))
         (compress-sexp #(a b a b a c c c c) (make-unit-weights 9) '((4 . 2)) '((3 . 0)) #t w)))
;; 窓幅で区間表が n×W に収まることの確認。時間は測らない
(test* "a long input runs within the window" #t
       (let* ([items (list->vector (map (cut modulo <> 5) (iota 600)))]
              [settings (apply make-repeat-tree-settings `(,@(list-ref oracle-settings 0) 16))])
         (receive (cost tree) (repeat-tree-compress items (make-unit-weights 600) eqv? settings)
           (and (< cost 600)
                (list? (repeat-tree-inspect items (make-unit-weights 600) eqv? settings tree cost))))))

;; If you don't want `gosh' to exit with nonzero status even if
;; the test fails, pass #f to :exit-on-failure.
(test-end :exit-on-failure #t)
