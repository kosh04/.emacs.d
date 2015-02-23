;;; init.el --- .emacs

;; Emacs24.4 から標準実装らしい
(or (fboundp 'with-eval-after-load)
    (defmacro with-eval-after-load (feature &rest body)
      (declare (indent 1))
      `(eval-after-load ,feature
         '(progn ,@body))))

(add-to-list 'load-path "~/Dropbox/GitHub/emacs-lisp/")

(load "config/package")

(load "site-lisp/cl-compatible")
(load "site-lisp/xyzzy")
(load "site-lisp/xyzzy-keymap")
(load "site-lisp/unicode-escape")
(load "site-lisp/google")
(load "site-lisp/misc")

(load "config/backup")
(load "config/basics")
(load "config/dired")
(load "config/disabled")
(load "config/display")
(load "config/emacs-lisp")
(load "config/encoding")
(load "config/git")
(load "config/history")
(load "config/keymaps")
(load "config/misc")
(load "config/progmode")
(load "config/sgml")
(load "config/shell")
(load "config/textedit")
(load "config/pkg-built-in")
(load "config/pkg-tabbar")

(use-package "config/osx" :if (eq system-type 'darwin))
(use-package "config/w32" :if (eq system-type 'windows-nt))

;;; init.el ends here
