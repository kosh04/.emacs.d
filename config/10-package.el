;;; config/package-util

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;;(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)

;; Switch to Mirrors
;; https://github.com/melpa/melpa#mirrors
;;(setf (cdr (assoc "melpa"        package-archives)) "https://www.mirrorservice.org/sites/melpa.org/packages/")
;;(setf (cdr (assoc "melpa-stable" package-archives)) "https://www.mirrorservice.org/sites/stable.melpa.org/packages/")

(custom-set-variables
 ;; アーカイブの優先順位 (it works?)
 '(package-archive-priorities
   '(("manual" . 99)
     ("melpa-stable" . 30)
     ("melpa" . 20)
     ("gnu" . 10))))

;;(package-initialize)

(with-eval-after-load "package"
  (define-key package-menu-mode-map (kbd "I") 'package-install)
  (define-key package-menu-mode-map (kbd "D") 'package-uninstall)
  (define-key package-menu-mode-map (kbd "?") 'describe-package)
  (define-key package-menu-mode-map (kbd "/") 'occur))

(defun user::package-uninstall (pkg)
  "Uninstall the package PKG."
  (interactive (list (intern (completing-read "Uninstall package: " package-alist))))
  (pcase-let ((`(,_name ,desc)
               (assq pkg package-alist)))
    (if (package-desc-p desc)
        (package-delete desc))))

(unless (symbol-function 'package-uninstall)
  (setf (symbol-function 'package-uninstall) #'user::package-uninstall))

;; TODO:
;; - try https://github.com/raxod502/straight.el
;; - :config (というかトップレベル以外) で補助関数を定義するとソースジャンプができない

;; use-package
(or (require 'use-package nil 'noerror)
    ;;(package-install 'use-package)
    (defmacro use-package (name &rest _args)
      "Dummy definition `use-package'."
      `(warn "Ignored Package: `%s'" ',name)))

(use-package use-package
  :pin #:melpa-stable
  :custom
  (use-package-verbose t)
  (use-package-enable-imenu-support t)
  (use-package-ignore-unknown-keywords t)
  (use-package-expand-minimally t)
  ;; 初回起動時は自動インストールしたい
  (use-package-always-ensure
   (if (getenv "TRAVIS") nil t))
  ;; 遅延ロード
  (use-package-always-defer t)
  )

;; modernizing Emacs Package Menu
(use-package paradox
  :pin #:melpa-stable
  :init (and (require 'paradox nil t)
             (paradox-enable))
  :custom
  (paradox-execute-asynchronously t)
  ;; "⛺" を使いたいが `tabulated-list' は今のところ emoji 非対応らしい
  (paradox-homepage-button-string "⌂")
  (paradox-column-width-version 13)     ; melpa 20190123.4567 (13 characters)
  )
