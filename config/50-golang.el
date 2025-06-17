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
  :hook
  (go-mode . user::lsp-enable)
  (go-mode . subword-mode)
  :config
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook (lambda () (setq compile-command "go build -v && go test -v && go vet")))
  :custom
  (go-play-browse-function #'browse-url)
  (gofmt-command "goimports")
  :bind (:map go-mode-map
              ("C-c h" . godoc)         ; or "C-c C-j"
              ))

(use-package go-eldoc
  :disabled
  :after go-mode
  :hook (go-mode . go-eldoc-setup))
  
(use-package company-go
  :disabled
  :after (go-mode company)
  :custom
  (company-go-insert-arguments nil)
  :config
  (add-to-list 'company-backends 'company-go))

(use-package golint
  :after go-mode)

(use-package go-guru
  :if (executable-find "guru")
  :after go-mode
  :hook (go-mode . go-guru-hl-identifier-mode))

;; go-playground client
;; https://github.com/kosh04/emacs-go-playground
(use-package go-playground-cli
  :load-path "~/Documents/GitHub/emacs-go-playground"
  :after go-mode
  :bind (:map go-mode-map
              ("C-c w w" . go-playground-cli-run-current-file)))

;; (workaround) "go tool vet" -> "go vet"
;; https://github.com/flycheck/flycheck/issues/1523#issuecomment-469402280
(with-eval-after-load 'flycheck
  (let ((govet (flycheck-checker-get 'go-vet 'command)))
    (when (equal (cadr govet) "tool")
      (setf (cdr govet) (cddr govet)))))
