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
      (insert (format "Translate: %s => %s\nProcessing Mode: %s\nOptimize level: 1\n\nProcessing Mode: 外部処理\n"
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
    (setq sxmml-compile-program-options '("-D" "variant" "-O" "-O" "1" "--mode=pmd" "--"))
    (sxmml-save-and-compile-buffer)
    (should (equal (sxmml-test-args) sxmml-compile-program-options))
    (setq sxmml-mode-settings '(("pmd-source" :compile-options ("-c" "pmd-compile"))))
    (sxmml-select-backend "pmd")
    (sxmml-select-optimization 0)
    (sxmml-save-and-compile-buffer)
    (should (equal (sxmml-test-args) '("-D" "variant" "-O" "-c" "pmd-compile" "-m" "pmd" "-O" "0" "--")))))

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

(ert-deftest sxmml-mode-autoplay-without-player ()
  (sxmml-test-with-song
    (let* ((completed nil)
           (sxmml-after-compile-hook (list (lambda () (setq completed t)))))
      (setq sxmml-play-after-compile t sxmml-mode-settings nil)
      (should (sxmml-save-and-compile-buffer))
      (should completed)
      (should (equal (mapcar #'car sxmml-test-calls) '("sxlc")))
      (should-error (sxmml-play-file) :type 'user-error))))

(ert-deftest sxmml-mode-autoplay-actual-mode-policy ()
  (sxmml-test-with-song
    (setq sxmml-mode-settings '(("fmp7" :player ("play-f") :play-after-compile nil)
                                ("pmd-binary" :player ("play-p") :play-after-compile t)))
    (setq sxmml-play-after-compile t)
    (sxmml-save-and-compile-buffer)
    (should (equal (mapcar #'car sxmml-test-calls) '("sxlc")))
    (sxmml-play-file)
    (should (equal (caar sxmml-test-calls) "play-f"))
    (sxmml-select-backend "fmp7")
    (setq sxmml-play-after-compile nil sxmml-test-mode "pmd-binary")
    (let ((sxmml-test-answers '("バックエンド" "pmd-binary")))
      (sxmml-save-and-compile-buffer t))
    (should (equal (caar sxmml-test-calls) "play-p"))))

(ert-deftest sxmml-mode-autoplay-toggle-and-inherit ()
  (sxmml-test-with-song
    (setq sxmml-test-mode "pmd-source"
          sxmml-mode-settings '(("pmd-source" :player ("play-p") :play-after-compile t)))
    (sxmml-select-backend "pmd")
    (let ((cmd (key-binding (kbd "C-c C-a"))))
      (call-interactively cmd)
      (should (local-variable-p 'sxmml-play-after-compile))
      (sxmml-save-and-compile-buffer)
      (should (equal (caar sxmml-test-calls) "sxlc"))
      (with-temp-buffer (should-not (local-variable-p 'sxmml-play-after-compile)))
      (let ((current-prefix-arg '(4))) (call-interactively cmd))
      (should-not (local-variable-p 'sxmml-play-after-compile))
      (sxmml-save-and-compile-buffer)
      (should (equal (caar sxmml-test-calls) "play-p")))))

(ert-deftest sxmml-mode-paths-for-compiler-and-player ()
  (sxmml-test-with-song
    (sxmml-save-and-compile-buffer)
    (should (equal (car (last (car sxmml-test-calls))) buffer-file-name))
    (let ((sxmml-compile-filename-function
           (lambda (path) (should (equal path buffer-file-name)) "/mnt/c/music/曲.sxl"))
          (sxmml-normalize-filename-function
           (lambda (path) (should (equal path "/mnt/c/out/別名.bin")) "c:/out/別名.bin")))
      (setq sxmml-test-output "/mnt/c/out/別名.mwi" sxmml-play-after-compile t)
      (sxmml-save-and-compile-buffer)
      (should (equal (car (last (cadr sxmml-test-calls))) "/mnt/c/music/曲.sxl"))
      (should (equal (car sxmml-test-calls) '("play-f" "-q" "c:/out/別名.bin"))))))
