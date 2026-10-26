;;;
;;; sxmml.utilの検査
;;;

(use gauche.test)

(test-start "sxmml.util")
(use sxmml.util)
(use sxmml.classes)
(test-module 'sxmml.util)

(test* "BPM換算後の既存の範囲違反は値エラーになる"
       (test-error <sxmml-value-error>
                   "invalid calculated Timer-B: -8411 (from BPM 1; expected an integer from 0 to 255)")
       (sxmml-bpm->timer-b 1 24))

(test* "正常なBPM換算結果を維持する"
       183
       (sxmml-bpm->timer-b 120 24))

(test-end :exit-on-failure #t)
