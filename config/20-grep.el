;;; config/Grep --- 検索いろいろ

;; (できれば) M-x grep,lgrep,rgrep を活用したい

;; Occur
(with-eval-after-load 'replace
  (let ((map occur-mode-map))
    (define-key map "n" 'occur-next)
    (define-key map "p" 'occur-prev))
  (add-hook 'occur-mode-hook 'next-error-follow-minor-mode))

(use-package ag
  :if (executable-find "ag")
  :custom
  (ag-highlight-search t)
  ;; :hook
  ;; (ag-mode . next-error-follow-minor-mode)
  :config
  ;;(add-to-list 'ag-arguments "--word-regexp")
  nil)
