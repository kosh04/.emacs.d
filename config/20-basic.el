;;; config/Basic

(setq kill-whole-line t)

;; 行の切り捨て (non-nil ならば行の折り返し無効)
(setf (default-value 'truncate-lines) t)

;; 入力補完で大文字小文字の区別をしない
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)

;; 補完候補がN以下ならば循環補完
(setq completion-cycle-threshold 3)

(add-to-list 'completion-ignored-extensions ".exe")

;; クリックで URL を開く
(add-hook 'text-mode-hook 'goto-address-mode)
(add-hook 'prog-mode-hook 'goto-address-prog-mode)

(url-handler-mode +1)

;; `what-cursor-position' [C-u C-x =]
(custom-set-variables
 '(describe-char-unicodedata-file
   (let ((unicode-data-txt (locate-user-emacs-file "UnicodeData.txt")))
     (if (file-exists-p unicode-data-txt)
         unicode-data-txt
         "http://www.unicode.org/Public/UNIDATA/UnicodeData.txt"))))

(setq use-dialog-box nil)
(setq print-quoted t)

;; C-u C-SPC C-SPC ...
(setq set-mark-command-repeat-pop t)
