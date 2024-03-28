;;; config/textedit

;; auto-fill
(setq fill-column 80)
;(add-hook 'latex-mode-hook 'turn-on-auto-fill)
;(add-hook 'text-mode-hook 'turn-on-auto-fill)
;(add-hook 'html-mode-hook 'turn-off-auto-fill)

;; M-x display-fill-column-indicator-mode

;; 折返しのないテキストを割と見かけるので
(add-hook 'text-mode-hook 'visual-line-mode)

;; プレーンテキストで貼り付け
;; (setq yank-excluded-properties t)

;; Time-stamp: <>
(use-package time-stamp
  :custom
  (time-stamp-format "%Y-%m-%dT%H:%M:%S%5z %l"))

;; Visualize the undo tree
(use-package vundo
  ;;     "C-x u" . undo
  :bind ("C-c u" . vundo))

;; NOTE: vundo vs undo-tree
;; https://www.reddit.com/r/emacs/comments/txwwfi/vundo_is_great_visual_undotree_for_emacs28/

;; Notepad++ はマルチ編集をフルサポート
;; https://forest.watch.impress.co.jp/docs/news/1549532.html
