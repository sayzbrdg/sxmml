;;;
;;; sxmml.utilの検査
;;;

(use gauche.test)

(test-start "sxmml.util")
(use sxmml.util)
(use sxmml.classes)
(test-module 'sxmml.util)

(test* "BPM換算後の既存の範囲違反は値エラーになる"
       (test-error <sxmml-value-error> #/timerb must range from 0 to 255/)
       (sxmml-bpm->timer-b 1 24))

(test-end :exit-on-failure #t)
