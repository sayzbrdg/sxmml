;; C 拡張を汎用圧縮だけで利用できることを確認する。
(use gauche.test)
(use gauche.process)
(use file.util)
(use srfi.13)
(use sxmml.native)
(test-start "sxmml.native")
(test-module 'sxmml.native)
(let1 root (sys-dirname (sys-dirname (current-load-path)))
  (test* "汎用圧縮はoptimizerを読み込まずに使える" "#t"
         (string-trim-right
          (process-output->string
           `(,(car (os-command-line)) ,(string-append "-I" root) "-e"
             "(use sxmml.repeat-tree) (receive (cost tree) (repeat-tree-compress #(a b a b a) #(1 1 1 1 1) eq? (make-repeat-tree-settings '() '((3 . 0)) #t 256)) (print (and (= cost 2) (final? tree) (not (find-module 'sxmml.optimizer)))))")))))
(test-end :exit-on-failure #t)
