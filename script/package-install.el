;;; package-install.el

;; Usage: $ emacs --script package-install.el

(warn "This script deprecated. use `elpa-cli.el'")

(require 'package)

(defvar user/packages
  '(names
    init-loader
    use-package
    let-alist
    request
    s
    f
    rainbow-identifiers
    )
  "初回起動時とsite-lispのバイトコンパイルに必要なパッケージ.")

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/"))
      package-archive-priorities
      '(("gnu" . 20)
        ("melpa-stable" . 10)
        ("melpa". 0))
      ;; package-pinned-packages ()
      )
(setq debug-on-error t)

(package-initialize)
(package-refresh-contents)

(dolist (x user/packages)
  (package-install x))
