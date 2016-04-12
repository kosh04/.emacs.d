;;; config/newlisp-mode

;; 開発版
(add-to-list 'load-path "~/Downloads/gitrepo/newlisp-mode/")

(use-package newlisp-mode
  :mode ("\\.lsp$" . newlisp-mode)
  :bind (:map newlisp-mode-map
              ("C-c h" . newlisp-lookup-manual))
  :config
  (add-hook 'newlisp-mode-hook 'eldoc-mode)
  (setq newlisp-manual-text "~/Dropbox/Public/newlisp/newlisp_manual.txt"))
