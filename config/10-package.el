;;; config/package

;; Package
(require 'package)
(require 'dash)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;;(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)

;;(package-initialize t)

(with-eval-after-load "package"
  (define-key package-menu-mode-map (kbd "I") 'package-install)
  (define-key package-menu-mode-map (kbd "D") 'package-uninstall)
  (define-key package-menu-mode-map (kbd "?") 'describe-package)
  (define-key package-menu-mode-map (kbd "/") 'occur)
  t)

(defun package-uninstall (pkg)
  "Uninstall the package PKG."
  (interactive (list (intern (completing-read "Uninstall package: " package-alist))))
  (-let (((_ pkg-desc)
          (assq pkg package-alist)))
    (if (package-desc-p pkg-desc)
        (package-delete pkg-desc))))

;; use-package
(or (require 'use-package nil t)
    (defmacro use-package (name &rest args)
      "Dummy definition `use-package'."
      `(message "Ignore Package: %s" ',name)))

(custom-set-variables
 '(use-package-verbose t)
 ;; 初回起動時は自動インストールしたい
 ;;'(use-package-always-ensure t)
 ;;'(use-package-always-defer t)
 )
