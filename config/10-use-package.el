;;; config/use-package

;; TODO:
;;   - try https://github.com/raxod502/straight.el
;;   - :config (というかトップレベル以外) で補助関数を定義するとソースジャンプができない

;; use-package
(or (require 'use-package nil t)
    ;;(package-install 'use-package)
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
