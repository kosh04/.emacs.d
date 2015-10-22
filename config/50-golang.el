;;; config/golang.el

(use-package go-mode
  :defer t
  :init (add-hook 'before-save-hook 'gofmt-before-save)
  :config (custom-set-variables
           '(go-play-browse-function #'browse-url)))

;; go-playground
;; https://github.com/kosh04/emacs-go-playground
(add-to-list 'load-path "~/Documents/GitHub/emacs-go-playground/")
(require 'go-playground nil t)

(with-eval-after-load "go-playground"
  (define-key go-mode-map (kbd "C-c w w") 'go-playground))
