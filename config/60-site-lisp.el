;;; config/site-lisp

(require 'unicode-escape)
(require 'textproc)

(require 'cl-compatible)
(require 'xyzzy)
(require 'xyzzy-keymap)
(require 'user-utils)

(require 'google-search)
(global-set-key (kbd "C-c g") 'google-search)