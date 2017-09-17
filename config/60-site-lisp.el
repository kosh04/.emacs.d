;;; config/site-lisp

(use-package unicode-escape
  :load-path "~/Documents/GitHub/unicode-escape.el"
  :pin #:manual)
(require 'textproc)
(require 'gitter-irc)
(require 'cl-compatible)
(require 'ssh-public-key-overlay)
(require 'xyzzy)
(require 'xyzzy-keymap)
(require 'user-utils)

(require 'google-search)
(global-set-key (kbd "C-c g") 'google-search)
