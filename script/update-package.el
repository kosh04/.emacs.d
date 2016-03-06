;;; batch-package.el

;;; Usage:

;; $ emacs --script package-upgrade.el

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)

(package-initialize t)

;;;###autoload
(defun command-line-package-update ()
  "Run package-update."
  (let (err)
    (condition-case e
        (progn
          (list-packages)
          (package-menu-mark-upgrades)
          (package-menu-execute :noquery))
      (error
       (message "Error: %s" (error-message-string e))
       (setq err t)))
    (kill-emacs (if err 1 0))))

;;(add-to-list 'command-switch-alist '("--update" . command-line-package-update))
(command-line-package-update)

;;; batch-package.el ends here.
