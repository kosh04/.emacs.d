;;; config/spell.el --- spell check

;; ispell/flyspell/aspell/hunspell/etc..
;; - http://aspell.net/
;; - https://hunspell.github.io

(setenv "DICTIONARY" "en_US")

;; Ispell
(require 'ispell)
(custom-set-variables
 ;;'(ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))
 ;;'(ispell-dictionary "american")
 ;;'(ispell-personal-dictionary (locate-user-emacs-file ".ispell"))
 )

;; flyspell
;;(add-hook 'c-mode-hook 'flyspell-prog-mode)

(use-package flymake-yaml
  :after yaml-mode
  :config (add-hook 'yaml-mode-hook 'flymake-yaml-load))

;; C-c ! v `flycheck-verify-setup'
(use-package flycheck
  :diminish flycheck-mode
  :custom
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  ;; SC1090 Can't follow non-constant source.
  (flycheck-shellcheck-excluded-warnings '("SC1090"))
  :hook
  (emacs-startup . global-flycheck-mode)
  ;;(global-flycheck-mode -1)
  :config
  (add-to-list 'flycheck-clang-include-path "/usr/include/")
  ;;(add-to-list 'flycheck-clang-include-path "/usr/local/include/")
  )

(use-package flycheck-pos-tip
  :disabled
  :after flycheck
  :demand t
  :config (flycheck-pos-tip-mode +1))

;; https://github.com/Wilfred/flycheck-title
