;;; init.el --- .emacs

(defvar my:user-init-directory
  (file-name-directory (or load-file-name ".")))

;;(setq default-directory my:user-init-directory)

;; Add current directory
(add-to-list 'load-path my:user-init-directory)

;; available in Emacs24.4+
(or (fboundp 'with-eval-after-load)
    (defmacro with-eval-after-load (feature &rest body)
      (declare (indent 1))
      `(eval-after-load ,feature
         '(progn ,@body))))

;; load config/nn-xxx.el
;; TODO: use init-loader
(dolist (config (directory-files 
                 (file-name-as-directory
                  (expand-file-name "config/" my:user-init-directory))
		 t "[[:digit:]]\\{2\\}+-.+\\.el\\'"))
  ;;(setq config (file-relative-name (file-name-sans-extension config) my:user-init-directory))
  (load config t))

(use-package "osx-conf" :if (eq system-type 'darwin))
(use-package "w32-conf" :if (eq system-type 'windows-nt))

;;; init.el ends here
