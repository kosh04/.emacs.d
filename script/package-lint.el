;;; package-lint.el --- package-lint as script

;;; Usage:

;; $ emacs --script package-lint FILENAME.el

(require 'package)
(package-initialize)

;; TODO: How to add more package-archives for "Package-Requires" keyword
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(require 'package-lint)
(package-lint-batch-and-exit)
