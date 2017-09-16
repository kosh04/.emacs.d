;;; package-install.el

;; Usage: $ emacs --script package-install.el

(require 'package)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/"))
      package-archive-priorities
      '(("melpa-stable" . 20)
        ("gnu" . 10)
        ("melpa". 0))
      ;; package-pinned-packages
      )

(package-initialize)

(defvar user/packages
  '(f s dash names init-loader use-package))

(dolist (x user/packages)
  (or (package-installed-p x)
      (package-install x)))
