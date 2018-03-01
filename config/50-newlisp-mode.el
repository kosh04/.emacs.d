;;; config/newlisp-mode

(use-package newlisp-mode
  ;; 開発版
  :load-path "~/src/gitrepo/newlisp-mode"
  :pin #:manual
  :mode ("\\.lsp\\'" . newlisp-mode)
  :bind (:map newlisp-mode-map
              ("C-c h" . newlisp-lookup-manual))
  :config
  (add-hook 'newlisp-mode-hook 'eldoc-mode)
  (setq newlisp-manual-text "~/Dropbox/Public/bookshelf/static/newlisp/newlisp_manual.txt"))
