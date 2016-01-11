;;; config/spell.el --- spell check

;; ispell/flyspell/aspell/hunspell/etc..

;; Ispell
(require 'ispell)
;; (custom-set-variables
;;  '(ispell-dictionary "american"))
;; (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))

;; 自動終了してくれないので一時的な対策 [2015-08-31]
(when (eq system-type 'windows-nt)
  (defun user:ispell-killall ()
    (ispell-kill-ispell t))
  (add-hook 'kill-emacs-hook 'user:ispell-killall))

(use-package flymake-yaml
  :defer t
  :config (add-hook 'yaml-mode-hook 'flymake-yaml-load)
  :ensure yaml-mode)

(use-package flycheck
  :defer t
  :init
  (custom-set-variables
   '(flycheck-emacs-lisp-load-path 'inherit)
   '(flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :ensure t)

(use-package flycheck-pos-tip
  :if window-system
  :config
  (custom-set-variables
   '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages))
  :ensure flycheck)
