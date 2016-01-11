;;; config/golang.el

(use-package go-mode
  :defer t
  :config
  (progn
    (add-hook 'before-save-hook 'gofmt-before-save)
    (custom-set-variables
     '(go-play-browse-function #'browse-url))))

;; go-playground client
;; https://github.com/kosh04/emacs-go-playground

;;(require 'go-playground-cli "~/Documents/GitHub/emacs-go-playground/go-playground-cli.el" t)

(with-eval-after-load 'go-playground-cli
  (define-key go-mode-map (kbd "C-c w w") 'go-playground-cli-run-current-file))
