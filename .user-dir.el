;; set current directory as "~/.emacs.d/"
;; `locate-user-emacs-file' depends on `user-emacs-directory'

;; NOTE: This code was obsoleted since Emacs 29.1
;; please use `--init-directory' option

(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))
