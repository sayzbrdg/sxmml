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
(define (ones n) (make-vector n 1))
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
       (repeat-tree-inspect #(0 1 0 1 0) (ones 5) eqv? s-final3
                            (make-final 3 (make-literal 0) (make-literal 1)) 2))
(test* "inspect counts weights" '(0 0)
       (repeat-tree-inspect #(a a) #(5 5) eq? s-repeat2 (make-repeat 2 (make-literal 0)) 7))
(dolist [c `(("a disallowed count" #(a a) #(5 5) ,s-repeat2 ,(make-repeat 3 (make-literal 0)) 7)
             ("a wrong cost" #(0 1 0 1 0) ,(ones 5) ,s-final3
              ,(make-final 3 (make-literal 0) (make-literal 1)) 3)
             ("a wrong expansion" #(0 1) ,(ones 2) ,s-repeat2 ,(make-repeat 2 (make-literal 0)) 3)
             ("an index out of range" #(0) ,(ones 1) ,s-repeat2 ,(make-literal 1) 1)
             ("a disabled final" #(0 1 0 1 0) ,(ones 5) ,(make-repeat-tree-settings '() '((3 . 0)) #f 256)
              ,(make-final 3 (make-literal 0) (make-literal 1)) 2)
             ;; 回数は許可されているが、展開列を作る前に長さで失敗する
             ("a too long expansion" #(0 0) ,(ones 2) ,(make-repeat-tree-settings '((255 . 0)) '() #f 256)
              ,(make-repeat 255 (make-literal 0)) 1)
             ("a non-tree value" #(0) ,(ones 1) ,s-repeat2 (lit 0) 1))]
  (test* #"inspect rejects ~(car c)" (test-error <error>)
         (repeat-tree-inspect (list-ref c 1) (list-ref c 2) eqv? (list-ref c 3) (list-ref c 4) (list-ref c 5))))

;; If you don't want `gosh' to exit with nonzero status even if
;; the test fails, pass #f to :exit-on-failure.
(test-end :exit-on-failure #t)
