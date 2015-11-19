;;; config/site-lisp.el

(require 'cl-compatible)
(require 'xyzzy)
(require 'xyzzy-keymap)
(require 'unicode-escape)
(require 'google-search)
(global-set-key (kbd "C-c g") 'google-search)
(require 'user-utils)
