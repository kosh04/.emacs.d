;;; config/spell.el --- spell check

;; ispell/flyspell/aspell/hunspell/etc..

;; Ispell
(require 'ispell)
(custom-set-variables
 ;;'(ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))
 ;;'(ispell-dictionary "american")
 ;;'(ispell-personal-dictionary (locate-user-emacs-file ".ispell"))
 )

;; 自動終了してくれないので一時的な対策 [2015-08-31]
(when (eq system-type 'windows-nt)
  (defun user:ispell-killall ()
    (ispell-kill-ispell t))
  (add-hook 'kill-emacs-hook 'user:ispell-killall))

;; flyspell
;;(add-hook 'c-mode-hook 'flyspell-prog-mode)

(use-package flymake-yaml
  :defer t
  :after yaml-mode
  :config (add-hook 'yaml-mode-hook 'flymake-yaml-load))

;; C-c ! v `flycheck-verify-setup'
(use-package flycheck
  :diminish flycheck-mode
  :custom
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  :hook
  (emacs-startup . global-flycheck-mode)
  ;;(global-flycheck-mode -1)
  :config
  (add-to-list 'flycheck-clang-include-path "/usr/include/")
  ;;(add-to-list 'flycheck-clang-include-path "/usr/local/include/")
  )

(use-package flycheck-pos-tip
  :after flycheck
  :config (flycheck-pos-tip-mode +1))

;; https://github.com/Wilfred/flycheck-title