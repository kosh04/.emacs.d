;; set current directory as "~/.emacs.d/"
;; `locate-user-emacs-file' depends on `user-emacs-directory'
;; TODO: ensure this code to eary-init.el
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))
