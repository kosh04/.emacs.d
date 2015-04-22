;;; init.el --- .emacs

(defvar my:user-init-directory
  (file-name-directory (or load-file-name ".")))

(add-to-list 'load-path my:user-init-directory)

;; available in Emacs24.4+
(or (fboundp 'with-eval-after-load)
    (defmacro with-eval-after-load (feature &rest body)
      (declare (indent 1))
      `(eval-after-load ,feature
         '(progn ,@body))))

;; for bootstrap init-loader
(require 'package)
(package-initialize)

;; load config/nn-xxx.el
(require 'init-loader)
(setq init-loader-directory
      (expand-file-name "config/" my:user-init-directory))
(init-loader-load)

;;; init.el ends here
