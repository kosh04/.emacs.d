;;; config/backup.el

(setq backup-by-copying t)

(setq make-backup-files t)
(setq backup-by-copying t)
;(setq version-control t)

;(add-to-list 'backup-directory-alist `("." . ,temporary-file-directory))
;(add-to-list 'backup-directory-alist `("." . ,(expand-file-name "backup" user-emacs-directory)))


;; EmacsWiki: Backup Directory - http://www.emacswiki.org/emacs/BackupDirectory
(defun my:make-backup-file-name (file)
  (let ((dirname (file-name-as-directory
                  (format-time-string
                   (expand-file-name "backup/%Y-%m-%d/" user-emacs-directory)))))
    (or (file-directory-p dirname)
        ;; mkdir -p DIRNAME
        (make-directory dirname t))
    (expand-file-name (file-name-nondirectory file) dirname)))

(setq make-backup-file-name-function #'my:make-backup-file-name)
