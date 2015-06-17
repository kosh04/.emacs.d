;;; config/basics.el

(setq kill-whole-line t)

;; 行の切り捨て (non-nil ならば行の折り返し無効)
(setf (default-value 'truncate-lines) t)

;; 入力補完で大文字小文字の区別をしない
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)

(url-handler-mode +1)

;; use what-cursor-position [C-u C-x =]
(setq describe-char-unicodedata-file "http://www.unicode.org/Public/UNIDATA/UnicodeData.txt")
