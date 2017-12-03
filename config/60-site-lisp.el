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

(use-package gitignore
  :load-path "~/Documents/GitHub/gitignore.el"
  :pin #:manual
  :config
  (set-variable
   'gitignore-template-directory
   (file-name-as-directory
    (locate-user-emacs-file "share/autoinsert/gitignore")))
  (with-eval-after-load 'gitignore-mode
    (define-key gitignore-mode-map [remap insert-file] #'gitignore-insert-template))
  )
