;;; config/newlisp-mode

;; 開発版
(add-to-list 'load-path "~/Downloads/gitrepo/newlisp-mode/")

(use-package newlisp-mode
  :config
  (progn
    (bind-keys :map newlisp-mode-map ("C-c h" . newlisp-lookup-manual))
    (setq newlisp-manual-text "~/Dropbox/Public/newlisp/newlisp_manual.txt")))
