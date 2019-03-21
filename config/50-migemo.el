;;; config/migemo

(use-package migemo
  ;;:if (executable-find "cmigemo")
  :hook (emacs-startup . migemo-init)
  :custom
  (migemo-command "cmigemo")
  (migemo-options '("-q" "--emacs"))
  (migemo-dictionary (expand-file-name
                      (locate-user-emacs-file
                       "share/dict/utf-8/migemo-dict")))
  (migemo-coding-system 'utf-8)  
  ;; キャッシュを有効にする
  ;;(migemo-use-pattern-alist t)
  ;;(migemo-use-frequent-pattern-alist t)
  ;;(migemo-pattern-alist-length 1024)
  )
