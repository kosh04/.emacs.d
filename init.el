;;; init.el --- .emacs

(custom-set-variables
 '(custom-file (locate-user-emacs-file "init-custom.el")))

(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

(add-to-list 'load-path (locate-user-emacs-file "site-lisp"))

;; available in Emacs24.4+
(or (fboundp 'with-eval-after-load)
    (defmacro with-eval-after-load (feature &rest body)
      "Execute BODY after FEATURE is loaded."
      (declare (indent 1) (debug t))
      `(eval-after-load ,feature
         (lambda () ,@body))))

;; for bootstrap init-loader
(require 'package)
(package-initialize)

;; load config/nn-xxx.el
(require 'init-loader)
(setq init-loader-directory (locate-user-emacs-file "config"))
(init-loader-load)

;;; init.el ends here
