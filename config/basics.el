;;; config/basics.el

;; pkg-built-in.el とあんまり変わらなくないか？

(setq kill-whole-line t)

;; gzファイルも編集できるように
(auto-compression-mode)
(add-hook 'tar-mode-hook 'hl-line-mode)
(add-hook 'tar-mode-hook 'read-only-mode)

;; 入力補完で大文字小文字の区別をしない
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)

;; Theme
;; (load-theme 'zenburn)
;; (load-theme 'wombat)

;; Buffer
(global-set-key (kbd "C-x C-b") 'bs-show)
(add-hook 'bs-mode-hook 'hl-line-mode)
(add-hook 'buffer-menu-mode-hook 'hl-line-mode)

(url-handler-mode)

;; use what-cursor-position [C-u C-x =]
(setq describe-char-unicodedata-file "http://www.unicode.org/Public/UNIDATA/UnicodeData.txt")
