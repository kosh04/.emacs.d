#!/usr/bin/env emacs

;; original source
;; https://github.com/purcell/emacs.d/blob/master/test-startup.sh

(let ((debug-on-error t)
      (user-init-file "./init.el")
      (user-emacs-directory default-directory)
      (load-path (delq default-directory load-path)))
  (load-file user-init-file)
  '(run-hooks 'after-init-hook))

(message "init time: %s" (emacs-init-time))
