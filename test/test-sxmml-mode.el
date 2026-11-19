;;; -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'sxmml-mode)

;; 外部コマンドとの境界で引数を記録し、sxlc と同じ形式の結果を返す。
(defvar sxmml-test-calls)
(defvar sxmml-test-answers)
(defvar sxmml-test-status)
(defvar sxmml-test-mode)
(defvar sxmml-test-output)

(defun sxmml-test-process (program _input destination _display &rest arguments)
  (push (cons program arguments) sxmml-test-calls)
  (when (bufferp destination)
    (with-current-buffer destination
      (insert (format "Translate: %s => %s\nProcessing Mode: %s\n"
                      (car (last arguments)) sxmml-test-output sxmml-test-mode))))
  sxmml-test-status)

(defmacro sxmml-test-with-song (&rest body)
  (declare (indent 0))
  `(with-temp-buffer
     (sxmml-mode)
     (setq buffer-file-name "/tmp/曲.sxl")
     (let ((sxmml-test-calls nil) (sxmml-test-status 0)
           (sxmml-test-mode "fmp7") (sxmml-test-output "/tmp/out/曲 alt.mwi")
           (sxmml-play-after-compile nil)
           (completing-read-function (lambda (&rest _) (pop sxmml-test-answers)))
           (sxmml-compile-program-options nil)
           (sxmml-mode-settings '(("fmp7" :player ("play-f" "-q") :play-extension ".bin")
                                  ("pmd-binary" :player ("play-p")))))
       (cl-letf (((symbol-function 'call-process) #'sxmml-test-process)
                 ((symbol-function 'display-buffer) #'ignore))
         ,@body))))

(defun sxmml-test-args ()
  (butlast (cdr (car sxmml-test-calls))))

(ert-deftest sxmml-mode-buffer-selections ()
  (sxmml-test-with-song
    (sxmml-select-backend "fmp7")
    (sxmml-select-optimization 2)
    (sxmml-select-backend "pmd-binary")
    (sxmml-save-and-compile-buffer)
    (should (equal (sxmml-test-args) '("-m" "pmd-binary" "-O" "2")))
    (with-temp-buffer
      (sxmml-mode)
      (setq buffer-file-name "/tmp/別の曲.sxl")
      (sxmml-save-and-compile-buffer)
      (should-not (sxmml-test-args)))))

(ert-deftest sxmml-mode-unspecified-and-zero ()
  (sxmml-test-with-song
    (setq sxmml-compile-program-options '("-D" "variant" "-O" "-O" "1" "--mode=pmd"))
    (sxmml-save-and-compile-buffer)
    (should (equal (sxmml-test-args) sxmml-compile-program-options))
    (sxmml-select-backend "fmp7")
    (sxmml-select-optimization 0)
    (sxmml-save-and-compile-buffer)
    (should (equal (sxmml-test-args) '("-D" "variant" "-O" "-m" "fmp7" "-O" "0")))))

(ert-deftest sxmml-mode-once ()
  (sxmml-test-with-song
    (sxmml-select-backend "fmp7")
    (sxmml-select-optimization 1)
    (dolist (item '((("バックエンド" "pmd-binary") "pmd-binary" "1")
                    (("最適化レベル" "9") "fmp7" "9")
                    (("両方" "pmd-binary" "0") "pmd-binary" "0")))
      (let ((sxmml-test-answers (copy-sequence (car item)))
            (current-prefix-arg '(4)))
        (call-interactively #'sxmml-save-and-compile-buffer))
      (should (equal (sxmml-test-args) (list "-m" (nth 1 item) "-O" (nth 2 item))))
      (sxmml-save-and-compile-buffer)
      (should (equal (sxmml-test-args) '("-m" "fmp7" "-O" "1"))))))

(ert-deftest sxmml-mode-play-actual-output ()
  (sxmml-test-with-song
    (setq sxmml-play-after-compile t)
    (sxmml-save-and-compile-buffer)
    (should (equal (car sxmml-test-calls) '("play-f" "-q" "/tmp/out/曲 alt.bin")))
    (sxmml-select-backend "pmd-binary")
    (sxmml-play-file)
    (should (equal (car sxmml-test-calls) '("play-f" "-q" "/tmp/out/曲 alt.bin")))
    (setq sxmml-test-mode "pmd-binary" sxmml-test-output "/tmp/別名.M")
    (sxmml-save-and-compile-buffer)
    (should (equal (car sxmml-test-calls) '("play-p" "/tmp/別名.M")))))

(ert-deftest sxmml-mode-postprocess-failure ()
  (sxmml-test-with-song
    (sxmml-save-and-compile-buffer)
    (setq sxmml-play-after-compile t sxmml-test-status 3
          sxmml-test-output "/tmp/失敗.mwi" sxmml-test-calls nil)
    (sxmml-save-and-compile-buffer)
    (should (equal (mapcar #'car sxmml-test-calls) '("sxlc")))
    (sxmml-play-file)
    (should (equal (car sxmml-test-calls) '("play-f" "-q" "/tmp/out/曲 alt.bin")))))
