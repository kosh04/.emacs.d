;;; config/archive.el

;; gzファイルも編集できるように
(auto-compression-mode +1)

(add-hook 'archive-mode-hook 'hl-line-mode)
(add-hook 'tar-mode-hook 'hl-line-mode)
