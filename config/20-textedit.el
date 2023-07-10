;;; config/textedit.el

;; auto-fill
(setq fill-column 80)
;(add-hook 'latex-mode-hook 'turn-on-auto-fill)
;(add-hook 'text-mode-hook 'turn-on-auto-fill)
;(add-hook 'html-mode-hook 'turn-off-auto-fill)

;; M-x display-fill-column-indicator-mode

;; プレーンテキストで貼り付け
;; (setq yank-excluded-properties t)

;; Time-stamp: <>
(use-package time-stamp
  :custom
  (time-stamp-format "%Y-%m-%dT%H:%M:%S%5z %l"))
