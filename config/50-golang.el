;;; config/golang.el

(use-package go-mode
  :defer t
  :config
  (progn
    (add-hook 'before-save-hook 'gofmt-before-save)
    (custom-set-variables
     '(go-play-browse-function #'browse-url))))

;; go-playground
;; https://github.com/kosh04/emacs-go-playground
(require 'go-playground "~/Documents/GitHub/emacs-go-playground/" t)

(with-eval-after-load 'go-playground
  (define-key go-mode-map (kbd "C-c w w") 'go-playground))

;; (require 'el-get)
;; (el-get-bundle! go-playground
;;   :url "https://github.com/kosh04/emacs-go-playground/raw/master/go-playground.el")

