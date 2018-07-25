;;; init.el --- .emacs

;; available in Emacs24.4+
(or (fboundp 'with-eval-after-load)
    (defmacro with-eval-after-load (feature &rest body)
      "Execute BODY after FEATURE is loaded."
      (declare (indent 1) (debug t))
      `(eval-after-load ,feature
         (lambda () ,@body))))

;; Enable installed packages
(package-initialize)

(add-to-list 'load-path (locate-user-emacs-file "site-lisp"))

;; Separate customization setting (do not overwrite `user-init-file')
(setq custom-file (locate-user-emacs-file "custom.el"))
;;(load custom-file t)

;; Load config/nn-xxx.el
(require 'init-loader)
(setq init-loader-directory (locate-user-emacs-file "config"))
(init-loader-load)

;;; init.el ends here
