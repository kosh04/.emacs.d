;;; package-install.el

;; Usage: $ emacs --script package-install.el

(require 'package)

(defvar user/packages
  '(names
    init-loader
    use-package))

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/"))
      package-archive-priorities
      '(("gnu" . 20)
        ("melpa-stable" . 10)
        ("melpa". 0))
      ;; package-pinned-packages
      )
(setq debug-on-error t)

(package-initialize)
(package-refresh-contents)

(dolist (x user/packages)
  (package-install x))
