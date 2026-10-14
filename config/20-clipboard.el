;;; config/clipboard.el

;; マウスで選択した領域をクリップボードに自動コピー
(setq mouse-drag-copy-region t)

;; (format "\e]52%s\e\\" "OSC 52")
;(use-package clipetty)

(setq kill-do-not-save-duplicates t)
