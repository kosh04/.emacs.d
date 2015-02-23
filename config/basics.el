;;; config/basics.el

;; pkg-built-in.el とあんまり変わらなくないか？

(setq kill-whole-line t)

;; auto-fill
(setq fill-column 74)
(add-hook 'latex-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'html-mode-hook 'turn-off-auto-fill)

;; gzファイルも編集できるように
(auto-compression-mode)
(add-hook 'tar-mode-hook 'hl-line-mode)
(add-hook 'tar-mode-hook 'read-only-mode)

;; 入力補完で大文字小文字の区別をしない
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)

;; 表示関係

;; バッファ末尾の可視化 (fringe)
;; (set-fringe-mode 5)
(setq-default indicate-empty-lines t)
(set-face-background 'fringe "gray80")

;(setq inhibit-startup-screen t)
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(menu-bar-mode)
(line-number-mode)
(column-number-mode)
(size-indication-mode)
(show-paren-mode)
(let ((system-time-locale "C"))
  (setq display-time-format "%Y-%m-%d(%a) %H:%M")
  (setq display-time-default-load-average nil
        display-time-24hr-format t
        display-time-day-and-date t)
  (display-time-mode))
(setq cursor-in-non-selected-windows t)

;; git-gutter と被る
;;(global-linum-mode)

;; モードライン
(set-face-background 'mode-line "black")
(set-face-foreground 'mode-line "gray95")

(setq frame-title-format
      `(" %b " (buffer-file-name "(%f)") " on " ,(system-name)
        " - " ,(format "Emacs %s" emacs-version)))

;; Theme
;; (load-theme 'zenburn)
;; (load-theme 'wombat)

;; Buffer
(global-set-key (kbd "C-x C-b") 'bs-show)
(add-hook 'bs-mode-hook 'hl-line-mode)
(add-hook 'buffer-menu-mode-hook 'hl-line-mode)

(url-handler-mode)

;; for what-cursor-position [C-u C-x =]
(setq describe-char-unicodedata-file "http://www.unicode.org/Public/UNIDATA/UnicodeData.txt")
