;;; 出力準備の検査
(use gauche.test)
(use sxmml)
(use sxmml.output)
(use srfi.1)

(test-start "sxmml.output")
(test-module 'sxmml.output)

(define (prepare env :key (output #f) (optimize 0) (source #f))
  (make-sxmml-output-environment env :output output :optimize optimize :source source))
(define (output-track env name)
  (hash-table-get (slot-ref env 'track) name))

(let* ([a (make <sxmml-mml-note> :value 0 :ticks 12)]
       [b (make <sxmml-mml-rest> :value #f :ticks 24)]
       [c (make <sxmml-mml-volume> :value 8)]
       [branch (list b c)] [tree (list a branch)]
       [result (sxmml-ir-flatten tree)])
  (test* "O01 順序を保ち、入力のペアとIRを変更しない"
         (list (list a b c) #t #t 24)
         (list result (eq? (cadr tree) branch) (eq? (car tree) a) (slot-ref b 'ticks)))
  (test* "O02 NOPと空の枝を除く"
         (list a b)
         (sxmml-ir-flatten (list () a (list (make <sxmml-mml-nop>) () b))))
  (let1 repeat-ir (list (make <sxmml-mml-repeat-start>) a
                       (make <sxmml-mml-repeat-break>) b
                       (make <sxmml-mml-repeat-end> :value 3))
    (test* "O03 反復命令をそのまま保つ" repeat-ir (sxmml-ir-flatten repeat-ir))))

(parameterize ([sxmml-environment (make <sxmml-environment>)])
  (define-track (A B C))
  (mml A "c2Lc1.")
  (mml B "c4")
  (mml C "c8c8d4")
  (let* ([registered (sxmml-environment)]
         [original (slot-ref (sxmml-track-get 'A) 'compiled-data)]
         [ticks (slot-ref (sxmml-track-get 'A) 'current-ticks)]
         [statistics (slot-ref (sxmml-track-get 'C) 'ticks-statistics)]
         [out (prepare registered)])
  (define (report name)
    (let1 track (output-track out name)
      (map (cut slot-ref track <>) '(total-ticks loop-ticks loop-exists))))
  (test* "O04 Lの前後から集計を確定する" '(192 144 #t) (report 'A))
  (test* "O05 Lがない場合の集計の意味を保つ" '(24 24 #f) (report 'B))
  (test* "O06 全登録トラックを準備する" '(A B C)
         (sort (hash-table-keys (slot-ref out 'track))))
  (test* "O07 登録状態へ書き戻さない" '(#t #t)
         (list (eq? original (slot-ref (sxmml-track-get 'A) 'compiled-data))
               (eq? ticks (slot-ref (sxmml-track-get 'A) 'current-ticks))))
  (let1 another (prepare registered :output "another.mml" :optimize 3)
    (test* "O08 同じ登録環境から異なる要求の出力環境を作れる"
           '(#f 0 "another.mml" 3)
           (list (slot-ref out 'output) (slot-ref out 'optimize)
                 (slot-ref another 'output) (slot-ref another 'optimize))))
  (parameterize ([sxmml-environment (make <sxmml-environment>)])
    (test* "O09 明示した登録環境を使う" '(A B C)
           (sort (hash-table-keys (slot-ref (prepare registered) 'track)))))
  (test* "O10 出力なしと入力元の省略を受理する" '(#f #f)
         (list (slot-ref out 'output) (slot-ref out 'source)))
  (test* "O12 分解能の初期値と解析時の統計を引き継ぐ" '(24 #t)
         (list (slot-ref out 'resolution)
               (eq? statistics (slot-ref (output-track out 'C) 'ticks-statistics))))
  (sxmml-track-rename! 'B 'lead)
  (let1 renamed (prepare registered)
    (test* "F01 partから名前変更後の出力トラックを得る" 'lead
           (slot-ref (sxmml-output-track-find renamed 'B) 'name))
    (test* "F02 見つからないpartは#f" #f (sxmml-output-track-find renamed 'missing)))))

(parameterize ([sxmml-environment (make <sxmml-environment>)])
  (define-track G)
  (mml G "[")
  (mml G "c8")
  (mml G "]3")
  (let1 track (output-track (prepare (sxmml-environment)) 'G)
    (test* "O11 登録呼出しをまたぐ反復を保つ" '(36 3)
           (list (slot-ref track 'total-ticks) (length (slot-ref track 'ir))))))

(for-each
 (^[case]
   (test* (car case) (cadr case)
          (guard (e [(sxmml-value-error? e) (condition-message e)]) ((caddr case)))))
 (list (list "D01 outputの省略は値エラー"
             "make-sxmml-output-environment requires :output"
             (^[] (make-sxmml-output-environment (make <sxmml-environment>) :optimize 0)))
       (list "D02 optimizeの省略は値エラー"
             "make-sxmml-output-environment requires :optimize"
             (^[] (make-sxmml-output-environment (make <sxmml-environment>) :output #f)))))

(test-end :exit-on-failure #t)
