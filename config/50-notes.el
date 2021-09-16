;;; config/notes

;; https://jblevins.org/projects/deft/
(use-package deft
  :bind ("<f7>" . deft)
  :custom
  (deft-directory "~/Documents/bookshelf/content/post/")
  (deft-extensions '("md" "org" "txt"))
  (deft-use-filename-as-title t)
  :config
  (define-advice deft-open-file (:after (file &optional _other _switch) read-only)
    (with-current-buffer (find-file-noselect file)
      (view-mode)))
  )
