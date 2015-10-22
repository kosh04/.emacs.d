;;; config/migemo.el

;;; Code:

(use-package migemo
  :config
  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs"))
  (setq migemo-dictionary (expand-file-name
                           (locate-user-emacs-file
                            "share/dict/utf-8/migemo-dict")))
  (setq migemo-coding-system 'utf-8)
  (migemo-init))
