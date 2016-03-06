;;; config/golang.el

(use-package go-mode
  :defer t
  :config
  (add-hook 'before-save-hook 'gofmt-before-save)
  (custom-set-variables
   '(go-play-browse-function #'browse-url))
  :bind (:map go-mode-map ("C-c h" . godoc)))

(use-package go-eldoc
  :config (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package company-go
  :config (add-to-list 'company-backends 'company-go))

;; go-playground client
;; https://github.com/kosh04/emacs-go-playground

(require 'go-playground-cli "~/Documents/GitHub/emacs-go-playground/go-playground-cli.el" t)

(with-eval-after-load 'go-playground-cli
  (define-key go-mode-map (kbd "C-c w w") 'go-playground-cli-run-current-file))
