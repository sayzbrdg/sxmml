;;;
;;; IRの意味比較器の検査
;;;

(use gauche.test)
(use sxmml.classes)
(use test.ir-meaning)

(test-start "test.ir-meaning")
(test-module 'test.ir-meaning)

(define (objects specs)
  (map (^[spec]
         (case (car spec)
           [(note) (make <sxmml-mml-note> :value (cadr spec) :ticks (caddr spec))]
           [(rest) (make <sxmml-mml-rest> :ticks (cadr spec))]
           [(volume) (make <sxmml-mml-volume> :value (cadr spec))]
           [(change) (make <sxmml-mml-volume-change> :value (cadr spec))]
           [(start) (make <sxmml-mml-repeat-start>)]
           [(break) (make <sxmml-mml-repeat-break>)]
           [(end) (make <sxmml-mml-repeat-end> :value (cadr spec))]
           [(loop) (make <sxmml-mml-loop>)]
           [(unsupported) (make <sxmml-mml-portamento> :value '(0 . 2) :ticks 12)]))
       specs))

(define (interpret specs)
  (call-with-values
    (^[] ((with-module test.ir-meaning %interpret-ir) (objects specs) 100000))
    list))

(for-each
  (^[case]
    (test* (car case) (caddr case) (interpret (cadr case))))
  '(("空の列の初期状態" ()
     (() 0 (initial-volume 0) #f #f))
    ("音符・休符と絶対／相対音量"
     ((volume 8) (note 0 12) (change 2) (rest 12) (note 2 12))
     (((0 0 12 8) (24 2 12 10)) 36 10 #f #f))
    ("未知の初期音量への正負の加算"
     ((change 2) (change -1) (note 0 12))
     (((0 0 12 (initial-volume 1))) 12 (initial-volume 1) #f #f))
    ("脱出を含む反復の実行順"
     ((volume 8) (start) (change 1) (note 0 12) (break) (change 1) (note 2 12) (end 2))
     (((0 0 12 9) (12 2 12 10) (24 0 12 11)) 36 11 #f #f))
    ("入れ子の反復と状態の引継ぎ"
     ((start) (change 1) (start) (note 0 12) (end 2) (end 2))
     (((0 0 12 (initial-volume 1)) (12 0 12 (initial-volume 1))
       (24 0 12 (initial-volume 2)) (36 0 12 (initial-volume 2)))
      48 (initial-volume 2) #f #f))
    ("1回の反復で脱出後を実行しない"
     ((start) (note 0 12) (break) (note 2 12) (end 1))
     (((0 0 12 (initial-volume 0))) 12 (initial-volume 0) #f #f))))

(define (compare left right :optional (step-limit 100000))
  (call-with-values
    (^[] (compare-ir-meaning left right :step-limit step-limit))
    list))

(for-each
  (^[case]
    (test* (car case) (list-ref case 3)
           (compare (objects (cadr case)) (objects (caddr case))
                    (if (pair? (cddddr case)) (list-ref case 4) 100000))))
  '(("相対指定の合成"
     ((change 1) (change 2) (note 0 12)) ((change 3) (note 0 12))
     (match #f))
    ("絶対指定と相対指定の合成"
     ((volume 8) (change 1) (note 0 12)) ((volume 9) (note 0 12))
     (match #f))
    ("休符の分割" ((rest 12) (rest 12)) ((rest 24)) (match #f))
    ("空の列の一致" () () (match #f))
    ("音符の開始tickの相違"
     ((rest 12) (note 0 12)) ((note 0 12) (rest 12))
     (mismatch (note 1 start-tick 12 0)))
    ("音高の相違" ((note 0 12)) ((note 2 12))
     (mismatch (note 1 pitch 0 2)))
    ("音長の相違" ((note 0 12) (rest 12)) ((note 0 24))
     (mismatch (note 1 ticks 12 24)))
    ("音符時の音量の相違"
     ((volume 8) (note 0 12) (volume 9)) ((volume 9) (note 0 12))
     (mismatch (note 1 volume 8 9)))
    ("終了時の総tickの相違" ((note 0 12) (rest 12)) ((note 0 12) (rest 24))
     (mismatch (end ticks 24 36)))
    ("終了時の音量の相違"
     ((volume 8) (note 0 12) (volume 9)) ((volume 8) (note 0 12))
     (mismatch (end volume 9 8)))
    ("初期状態に対する効果の相違" ((change 1) (note 0 12)) ((volume 8) (note 0 12))
     (mismatch (note 1 volume (initial-volume 1) 8)))
    ("完了した列への追加音符" () ((note 0 12))
     (mismatch (note 1 presence #f #t)))
    ("未対応IRより前の確定した相違"
     ((volume 8) (note 0 12) (unsupported)) ((volume 9) (note 0 12) (unsupported))
     (mismatch (note 1 volume 8 9)))
    ("Lがあっても終了状態の相違を判定"
     ((volume 8) (loop) (note 0 12) (volume 9)) ((volume 8) (loop) (note 0 12))
     (mismatch (end volume 9 8)))
    ("未対応の派生クラスと未知の後続"
     ((note 0 12) (unsupported)) ((note 0 12) (note 2 12))
     (undecided (unsupported-ir left 2 <sxmml-mml-portamento>)))
    ("時間が進まない反復の実行上限" () ((start) (change 1) (end 1000))
     (undecided (step-limit right 5)) 5)
    ("一巡が一致してもL全体は未判定"
     ((volume 8) (loop) (note 0 12) (volume 9))
     ((volume 8) (loop) (volume 8) (note 0 12) (volume 9))
     (undecided (loop)))
    ("停止時刻より前の音符の欠落"
     ((rest 24) (volume 8)) ((note 0 12))
     (mismatch (note 1 presence #f #t)) 1)))

(let1 shared (objects '((unsupported)))
  (test* "同一の未対応IRも読み飛ばさない"
         '(undecided (unsupported-ir left 1 <sxmml-mml-portamento>))
         (compare shared shared)))

(test-end :exit-on-failure #t)
