;;; config/package.el

;; Package
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
;;(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t) ; OLD
;;(add-to-list 'package-archives '("marmalade" . "http://melpa-stable.milkbox.net/packages/") t)
;;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(add-hook 'package-menu-mode-hook 'hl-line-mode)

(with-eval-after-load "package"
  (define-key package-menu-mode-map (kbd "?") 'package-menu-quick-help)
  t)

;; use-package
(or (require 'use-package nil t)
    (defmacro use-package (name &rest args)
      "Dummy definition `use-package'."
      `(message "Ignore Package: %s" ',name)))

(custom-set-variables
 '(use-package-verbose t))

;; Cask - Project management for Emacs package development
(use-package cask
  :defer t
  :config (cask-initialize))

