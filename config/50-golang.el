;;; config/Go

;; 事前に補助コマンドをインストールしておく
;; $ go get -u github.com/nsf/gocode (コード補完)
;; $ go get -u github.com/rogpeppe/godef (定義ジャンプ)
;; $ go get -u golang.org/x/tools/cmd/goimports (import をいい感じにする)

;; [2019-05-24] 追記
;; $ go get -u golang.org/x/tools/cmd/gopls
;; $ go get -u github.com/sourcegraph/go-langserver
;; $ go get -u golang.org/x/tools/cmd/goimports

;; TODO: Language Server 周りの整理

(use-package go-mode
  :defer t
  :config
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook 'subword-mode)
  :custom
  (go-play-browse-function #'browse-url)
  (gofmt-command "goimports")
  :bind (:map go-mode-map
              ("C-c h" . godoc)         ; or "C-c C-j"
              ;;("M-." . godef-jump)
              ))

(use-package go-eldoc
  :disabled t
  :after go-mode
  :init (add-hook 'go-mode-hook 'go-eldoc-setup))
  
(use-package company-go
  :disabled
  :after (go-mode company)
  :config
  ;; (custom-set-variables
  ;;  '(company-go-insert-arguments nil))
  (add-to-list 'company-backends 'company-go))

(use-package golint
  :after go-mode)

(use-package go-guru
  :if (executable-find "guru")
  :after go-mode
  :config (add-hook 'go-mode-hook 'go-guru-hl-identifier-mode))

;; go-playground client
;; https://github.com/kosh04/emacs-go-playground
(use-package go-playground-cli
  :load-path "../site-lisp/go-playground-cli"
  :after go-mode
  :bind (:map go-mode-map
              ("C-c w w" . go-playground-cli-run-current-file)))
