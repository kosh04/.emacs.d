;;; config/Backup

(setq make-backup-files t)

(setq backup-by-copying t)
;(setq version-control t)

;;(add-to-list 'backup-directory-alist (cons "." temporary-file-directory))
;;(add-to-list 'backup-directory-alist (cons "." (locate-user-emacs-file "backup")))

;; EmacsWiki: Backup Directory - http://www.emacswiki.org/emacs/BackupDirectory
(defun my:make-backup-file-name (file)
  "Create backup files on ~/.emacs.d/var/backup/YY-MM-DD/."
  (let ((dirname (file-name-as-directory
                  (locate-user-emacs-file
                   (format-time-string "var/backup/%Y-%m-%d/")))))
    (or (file-directory-p dirname)
        ;; mkdir -p DIRNAME
        (make-directory dirname t))
    (expand-file-name (file-name-nondirectory file) dirname)))

(setq make-backup-file-name-function #'my:make-backup-file-name)
