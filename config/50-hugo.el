;;; config/Hugo

;; M-x easy-hugo
(use-package easy-hugo
  :config
  (setq easy-hugo-basedir "~/Dropbox/Documents/bookshelf")
  (global-set-key (kbd "C-x t h") 'easy-hugo))
