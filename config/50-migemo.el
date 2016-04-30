;;; config/migemo

(use-package migemo
  :config
  (custom-set-variables
   '(migemo-command "cmigemo")
   '(migemo-options '("-q" "--emacs"))
   '(migemo-dictionary (expand-file-name
                        (locate-user-emacs-file
                         "share/dict/utf-8/migemo-dict")))
   '(migemo-coding-system 'utf-8))
  (migemo-init))
