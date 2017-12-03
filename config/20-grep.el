;;; config/Grep --- 検索いろいろ

;; (できれば) M-x grep,lgrep,rgrep を活用したい

(use-package ag
  :config
  (set-variable 'ag-highlight-search t)
  (add-to-list 'ag-arguments "--word-regexp")
  ;;(add-hook 'ag-mode-hook 'next-error-follow-minor-mode)
  )
