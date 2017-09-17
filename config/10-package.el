;;; config/package-util

(require 'package)
(require 'dash)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;;(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)

(custom-set-variables
 ;; アーカイブの優先順位 (it works?)
 '(package-archive-priorities
   '(("melpa-stable" . 30)
     ("melpa" . 20)
     ("gnu" . 10))))

;;(package-initialize)

(with-eval-after-load "package"
  (define-key package-menu-mode-map (kbd "I") 'package-install)
  (define-key package-menu-mode-map (kbd "D") 'package-uninstall)
  (define-key package-menu-mode-map (kbd "?") 'describe-package)
  (define-key package-menu-mode-map (kbd "/") 'occur))

(defun package-uninstall (pkg)
  "Uninstall the package PKG."
  (interactive (list (intern (completing-read "Uninstall package: " package-alist))))
  (-let (((_ pkg-desc)
          (assq pkg package-alist)))
    (if (package-desc-p pkg-desc)
        (package-delete pkg-desc))))
