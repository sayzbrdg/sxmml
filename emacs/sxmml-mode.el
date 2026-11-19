;;; -*- lexical-binding: t; -*-
;;; Copyright (c) 2024 Seiji Ohashi <sayzbrdg@gmail.com>
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;  1. Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;  2. Redistributions in binary form must reproduce the above copyright
;;;     notice, this list of conditions and the following disclaimer in the
;;;     documentation and/or other materials provided with the distribution.
;;;  3. Neither the name of the authors nor the names of its contributors
;;;     may be used to endorse or promote products derived from this
;;;     software without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;; HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;; TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(require 'scheme)
(require 'subr-x)

(defconst sxmml-font-lock-keywords
  (eval-when-compile
    (list
     (list "(\\(sxmml-\\(resolution\\|mode\\)\\)" 1 font-lock-keyword-face)
     (list (concat "(\\(define-\\(lfo\\|envelope\\|metadata\\|opn-voice\\)\\)"
                   "[ \t\n]*"
                   "\\(\\sw+\\)")
           '(1 font-lock-keyword-face)
           '(3 font-lock-variable-name-face))
     (list (concat "(\\(mml\\(-delay\\|-echo\\|-repeat\\)?\\)"
                   "[ \t\n]*"
                   "\\((\\(\\(\\sw+[ \t]*\\)+\\)\\))")
           '(1 font-lock-builtin-face)
           '(4 font-lock-type-face))
     (list (concat "(\\(mml\\(-delay\\|-echo\\|-repeat\\)?\\)"
                   "[ \t\n]*"
                   "\\(\\sw+\\)")
           '(1 font-lock-builtin-face)
           '(3 font-lock-type-face))
     (list (concat "(\\(rhythm\\)"
                   "[ \t\n]*"
                   "\\(\\sw+\\)")
           '(1 font-lock-builtin-face)
           '(2 font-lock-type-face))))
  "expressions to highlight in sxmml mode")

(defgroup sxmml nil "sxmml の編集とコンパイル。" :group 'languages)

(defcustom sxmml-default-mode nil
  "通常使う sxlc のモード。nil ならモードを指定しない。"
  :type '(choice (const nil) string))

(defcustom sxmml-default-optimization nil
  "通常使う最適化レベル。nil なら設定済みの引数に従う。"
  :type '(choice (const nil) natnum))

(defcustom sxmml-mode-settings nil
  "モード名と :compile-options、:player、:play-extension の設定表。
:player はプログラム名と引数のリスト。拡張子には先頭の点を含める。"
  :type '(alist :key-type string :value-type plist))

(defcustom sxmml-compile-program-name "sxlc"
  "コンパイラのプログラム名。" :type 'string)
(defcustom sxmml-compile-program-options nil
  "全モード共通の sxlc 引数。" :type '(repeat string))
(defcustom sxmml-play-after-compile nil
  "コンパイルが成功したとき続けて再生する。" :type 'boolean)
(defcustom sxmml-after-compile-hook nil
  "コンパイル成功時に曲のバッファで実行するフック。" :type 'hook)
(defcustom sxmml-normalize-filename-function #'convert-standard-filename
  "プレイヤーに渡すファイル名の変換関数。" :type 'function)

(defvar-local sxmml--last-output nil
  "最後に成功したコンパイルの (モード名 . 出力パス)。")

(define-derived-mode sxmml-mode scheme-mode "sxmml"
  "Scheme で記述する sxmml 楽曲の編集モード。"
  (font-lock-add-keywords nil sxmml-font-lock-keywords))
(add-to-list 'auto-mode-alist '("\\.sxl\\w*\\'" . sxmml-mode))

(defconst sxmml--unspecified "指定なし")

(defun sxmml--read-backend ()
  (let ((name (completing-read "バックエンド: "
                               (cons sxmml--unspecified (mapcar #'car sxmml-mode-settings))
                               nil nil nil nil (or sxmml-default-mode sxmml--unspecified))))
    (unless (equal name sxmml--unspecified) name)))

(defun sxmml--read-optimization ()
  (let ((value (completing-read "最適化レベル: " (cons sxmml--unspecified '("0" "1" "2" "3" "4" "9"))
                                nil nil nil nil (if sxmml-default-optimization
                                                    (number-to-string sxmml-default-optimization)
                                                  sxmml--unspecified))))
    (unless (equal value sxmml--unspecified)
      (unless (string-match-p "\\`[0-9]+\\'" value) (user-error "非負の整数を指定してください"))
      (string-to-number value))))

(defun sxmml-select-backend (mode)
  "このバッファの通常のバックエンドを選ぶ。"
  (interactive (list (sxmml--read-backend)))
  (setq-local sxmml-default-mode mode))

(defun sxmml-select-optimization (level)
  "このバッファの通常の最適化レベルを選ぶ。"
  (interactive (list (sxmml--read-optimization)))
  (setq-local sxmml-default-optimization level))

(defun sxmml--compile-options (mode level)
  (let* ((settings (cdr (assoc mode sxmml-mode-settings)))
         (options (append sxmml-compile-program-options (plist-get settings :compile-options)))
         kept)
    ;; -D の値や -c のコマンド文字列も単独のオプションに似た綴りを取り得る。
    (while (and options (not (equal (car options) "--")))
      (let* ((option (pop options))
             (key (cond ((string-match "\\`\\(--[^=]+\\)" option) (match-string 1 option))
                        ((string-match "\\`\\(-[a-zA-Z]\\)" option) (match-string 1 option))))
             (arity (cond ((member key '("-D" "--define")) 2)
                          ((member key '("-m" "--mode" "-O" "--optimize" "-c" "--command"
                                         "-j" "--jobs" "-o" "--output")) 1)
                          (t 0)))
             (replace (or (and mode (member key '("-m" "--mode")))
                          (and level (member key '("-O" "--optimize"))))))
        (unless replace (push option kept))
        (dotimes (_ (if (equal option key) arity (max 0 (1- arity))))
          (when options
            (let ((arg (pop options))) (unless replace (push arg kept)))))))
    (append (nreverse kept) (and mode (list "-m" mode))
            (and level (list "-O" (number-to-string level))) options)))

(defun sxmml--read-result (buffer source)
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-max))
      (when (re-search-backward "^Processing Mode: \\(.+\\)$" nil t)
        (let ((mode (match-string-no-properties 1)))
          (forward-line -1)
          (when (looking-at (concat "Translate: " (regexp-quote source) " => \\(.+\\)$"))
            (cons mode (match-string-no-properties 1))))))))

(defun sxmml-play-file ()
  "このバッファで最後に成功したコンパイルの結果を再生する。"
  (interactive)
  (unless sxmml--last-output (user-error "再生できるコンパイル結果がありません"))
  (let* ((settings (cdr (assoc (car sxmml--last-output) sxmml-mode-settings)))
         (player (plist-get settings :player))
         (output (cdr sxmml--last-output))
         (extension (plist-get settings :play-extension))
         (file (if extension (concat (file-name-sans-extension output) extension) output)))
    (unless player (user-error "%s のプレイヤーが未設定です" (car sxmml--last-output)))
    (apply #'call-process (car player) nil 0 nil
           (append (cdr player) (list (funcall sxmml-normalize-filename-function file))))))

(defun sxmml-save-and-compile-buffer (&optional once)
  "曲を保存してコンパイルする。前置引数 ONCE で今回だけ設定を変える。"
  (interactive "P")
  (unless buffer-file-name (user-error "バッファをファイルに保存してください"))
  (let* ((what (and once (completing-read "今回変更する項目: "
                                          '("バックエンド" "最適化レベル" "両方") nil t)))
         (mode (if (member what '("バックエンド" "両方")) (sxmml--read-backend) sxmml-default-mode))
         (level (if (member what '("最適化レベル" "両方")) (sxmml--read-optimization)
                  sxmml-default-optimization))
         (log (get-buffer-create "*sxmml-compilation*"))
         (source (expand-file-name buffer-file-name)))
    (save-buffer)
    (with-current-buffer log
      (let ((inhibit-read-only t)) (erase-buffer))
      (special-mode))
    (display-buffer log)
    (let ((status (let ((inhibit-read-only t))
                    (apply #'call-process sxmml-compile-program-name nil log nil
                           (append (sxmml--compile-options mode level) (list source))))))
      (if (equal status 0)
          (progn
            (setq sxmml--last-output (sxmml--read-result log source))
            (run-hooks 'sxmml-after-compile-hook)
            (when (and sxmml-play-after-compile sxmml--last-output) (sxmml-play-file))
            t)
        (message "sxlc のコンパイルに失敗しました (%s)" status)
        nil))))

(define-key sxmml-mode-map (kbd "C-c C-c") #'sxmml-save-and-compile-buffer)
(define-key sxmml-mode-map (kbd "C-c C-m") #'sxmml-select-backend)
(define-key sxmml-mode-map (kbd "C-c C-o") #'sxmml-select-optimization)
(define-key sxmml-mode-map (kbd "C-c C-p") #'sxmml-play-file)
(provide 'sxmml-mode)
