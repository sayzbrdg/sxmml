(use gauche.test)
(use sxmml)
(use test.ir-meaning)
(test-start "sxmml.backend.fmp7")
(use sxmml.backend.fmp7)
(test-module 'sxmml.backend.fmp7)

(define (prepared-tracks setup)
  (parameterize ([sxmml-environment (make <sxmml-environment>)])
    (setup)
    (sxmml-fmp7-tracks-prepare
     (make-sxmml-output-environment (sxmml-environment) :output #f :optimize 0))))

(test* "明示パートを予約して名前順に空きを割り当てる" '(("A0" fixed) ("A1" lead) ("A2" pad))
       (map (^[entry] (list (cadr entry) (slot-ref (car entry) 'name)))
            (prepared-tracks
             (^[] (define-track pad :sound-source 'pcm)
                  (define-track lead :sound-source 'opm)
                  (define-track fixed :part 'A :sound-source 'ssg)))))
(test* "AとA0の衝突" (test-error <sxmml-value-error>)
       (prepared-tracks (^[] (define-track x :part 'A :sound-source 'ssg)
                             (define-track y :part 'A0 :sound-source 'ssg))))
(test* "音源未指定を拒否する" (test-error <sxmml-value-error>)
       (prepared-tracks (^[] (define-track lead))))
(define (many-tracks counts)
  (prepared-tracks
   (^[] (for-each (^[kind count]
                    (dotimes [i count]
                      (sxmml-track-set! (string->symbol #"~|kind|~i") :sound-source kind)))
                  '(ssg pcm opm) counts))))
(test* "同じ音源の33トラックを拒否する" (test-error <sxmml-value-error>) (many-tracks '(33 0 0)))
(test* "合計65トラックを拒否する" (test-error <sxmml-value-error>) (many-tracks '(32 32 1)))
(test* "64個の空トラックは未指定partを保つ" '(64 #t)
       (let1 tracks (many-tracks '(32 32 0))
         (list (length tracks) (every (^[t] (not (slot-ref (car t) 'part))) tracks))))

(let* ([cd (map (^[pitch] (make <sxmml-mml-note> :ticks 12 :value pitch)) '(48 50))]
       [start (make <sxmml-mml-repeat-start>)] [end (make <sxmml-mml-repeat-end> :value 2)]
       [flat (apply append (make-list 4 cd))]
       [block (append (list start) cd (list end) cd)])
  (let1 compressed (sxmml-fmp7-ir-optimize flat 3)
    (test* "O3は同じ発音を短い反復へ変換する" '(#t match)
           (list (< (length compressed) (length flat)) (compare-ir-meaning flat compressed))))
  (let ([o3 (sxmml-fmp7-ir-optimize block 3)] [o4 (sxmml-fmp7-ir-optimize block 4)])
    (test* "O4は既存の反復を展開して再圧縮する" '(#t match match)
           (list (< (length o4) (length o3)) (compare-ir-meaning block o3) (compare-ir-meaning block o4))))
  (test* "開始のない反復" (test-error <sxmml-value-error>) (sxmml-fmp7-ir-optimize (list end) 3))
  (test* "回数0の反復" (test-error <sxmml-value-error>)
         (sxmml-fmp7-ir-optimize (list start (make <sxmml-mml-repeat-end> :value 0)) 3))
  (test* "33段の既存反復を受理する" #t
         (pair? (sxmml-fmp7-ir-optimize (append (make-list 33 start) cd (make-list 33 end)) 3))))

(test-end :exit-on-failure #t)
