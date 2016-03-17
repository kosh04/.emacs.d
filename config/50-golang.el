;;; config/Go

;; 事前に補助コマンドをインストールしておく
;; $ go get -u github.com/nsf/gocode (コード補完)
;; $ go get -u github.com/rogpeppe/godef (定義ジャンプ)
;; $ go get -u golang.org/x/tools/cmd/goimports (import をいい感じにする)

(use-package go-mode
  :defer t
  :config
  (add-hook 'before-save-hook 'gofmt-before-save)
  (custom-set-variables
   '(go-play-browse-function #'browse-url)
   '(gofmt-command "goimports"))
  :bind (:map go-mode-map
              ("C-c h" . godoc)         ; or "C-c C-j"
              ("M-." . godef-jump)
              ))

(use-package go-eldoc
  :config (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package company-go
  :config (add-to-list 'company-backends 'company-go))

;; go-playground client
;; https://github.com/kosh04/emacs-go-playground

(require 'go-playground-cli "~/Documents/GitHub/emacs-go-playground/go-playground-cli.el" t)

(with-eval-after-load 'go-playground-cli
  (define-key go-mode-map (kbd "C-c w w") 'go-playground-cli-run-current-file))
