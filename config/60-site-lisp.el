;;; config/site-lisp.el

(require 'cl-compatible)
(require 'xyzzy)
(require 'xyzzy-keymap)
(require 'unicode-escape)
(use-package google
  :bind (("C-c g" . google-search)))
(require 'user-utils)
