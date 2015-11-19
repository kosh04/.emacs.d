;;; config/display.el --- 表示関係

;(setq inhibit-startup-screen t)

;; 24.3.1@darwin にて未定義
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(menu-bar-mode)
(show-paren-mode)

;; Frame
;;(add-to-list 'default-frame-alist '(alpha . (0.75 0.75)))
(set-frame-parameter nil 'alpha 90)

;; アクティブでないウィンドウのカーソルを表示/非表示
;;(setq-default cursor-in-non-selected-windows nil)

;; git-gutter と被る
;;(global-linum-mode)

(setq frame-title-format
      `(" %b " (buffer-file-name "(%f)") " on " ,(system-name)
        " - " ,(format "Emacs %s" emacs-version)))

;; モードライン
(line-number-mode)
(column-number-mode)
(size-indication-mode)
;(display-battery-mode t)

(set-face-background 'mode-line "black")
(set-face-foreground 'mode-line "gray95")

(use-package nyan-mode
  :if window-system
  :config (progn
            (setq nyan-bar-length 12)
            (nyan-mode +1))
  :ensure t)

;; バッファ末尾の可視化 (fringe)
;; (set-fringe-mode 5)
(setq-default indicate-empty-lines t)
(set-face-background 'fringe "gray80")

;; 行カーソル
(require 'hl-line)
(set-face-background 'hl-line "#DEEDFF")
(add-hook 'help-mode-hook 'hl-line-mode)

(add-hook 'tabulated-list-mode-hook 'hl-line-mode)
(add-hook 'finder-mode-hook 'hl-line-mode)
