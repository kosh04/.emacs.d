;;; config/migemo

;; NOTE: M-m `migemo-toggle-isearch-enable'
(use-package migemo
  :if (executable-find "cmigemo")
  :hook (emacs-startup . migemo-init)
  :custom
  (migemo-command "cmigemo")
  (migemo-options '("-q" "--emacs" "-i" "\a")) ;??
  (migemo-isearch-enable-p nil)		; 必要に応じて有効化させる
  (migemo-dictionary
   (let ((dicts '("/usr/share/cmigemo/utf-8/migemo-dict"      ; apt
                  "/usr/local/share/migemo/utf-8/migemo-dict" ; homebrew
                  "~/.emacs.d/share/dict/utf-8/migemo-dict")) ; else
         (truename (lambda (dict)
                     (if (file-exists-p dict)
                         (file-truename dict)))))
     (seq-some truename dicts)))
  (migemo-coding-system 'utf-8)  
  ;; キャッシュを有効にする
  (migemo-use-pattern-alist t)
  (migemo-pattern-alist-file (locate-user-emacs-file "cache/migemo-pattern"))
  (migemo-use-frequent-pattern-alist t)
  ;;(migemo-pattern-alist-length 1024)
  )

(use-package ctrlf
  :disabled
  :custom
  (ctrlf-mode +1))
