;;; config/display.el --- 表示関係

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
