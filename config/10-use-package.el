;;; config/use-package

;; use-package
(or (require 'use-package nil t)
    (defmacro use-package (name &rest args)
      "Dummy definition `use-package'."
      `(message "Ignore Package: %s" ',name)))

(custom-set-variables
 '(use-package-verbose t)
 '(use-package-enable-imenu-support t)
 ;; 初回起動時は自動インストールしたい
 ;;'(use-package-always-ensure t)
 ;;'(use-package-always-defer t)
 )
