;; set current directory as "~/.emacs.d/"
;; `locate-user-emacs-file' depends on `user-emacs-directory'
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))
