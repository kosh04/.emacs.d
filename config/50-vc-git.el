;;; config/vc-git --- version control systems

;; TODO:
;; - wdired モードで `vc-rename-file' を 使いたい
;; - shell-mode で利用する Git コマンドでは PAGER を無効化したい
;;   * git --no-pager $*
;;   * git config core.pager cat 

(setf (symbol-function 'git-grep) #'vc-git-grep)
(setf (getenv "GIT_PAGER") "cat")

(defun user:open-repository-url (&optional action)
  "現在開いているgitリポジトリのリモートURLを表示します."
  (interactive)
  (unless action
    (setq action #'browse-url))
  (let ((url (with-temp-buffer
               (vc-git--call t "ls-remote" "--get-url")
               (string-trim (buffer-string)))))
    (message "%s" url)
    ;; ssh protocol -> https
    (when (string-match "git@\\(.+\\):\\(.+?\\)\\(\\.wiki\\)?\\(?:\\.git\\)?\\'" url)
      (setq url (format "https://%s/%s%s"
                        (match-string 1 url)
                        (match-string 2 url)
                        (if (match-string 3) "/wiki" ""))))
    (funcall action url)))

;; Magit
;; magit-auto-revert-mode によるプチフリーズに注意 (特に NTEmacs)
(use-package magit
  ;;:if (executable-find "git")
  :defer t
  :pin #:melpa-stable
  :bind (("C-x g" . magit-status)
         ("C-x t g" . magit-list-repositories)
         :map magit-mode-map
         ("&" . user:open-repository-url)
         ("!" . magit-git-command)      ; !!
         (":" . magit-dispatch-popup)   ; ?
         )
  :config
  (add-to-list 'magit-no-confirm 'stage-all-changes)
  ;; see [$] `magit-process'
  ;; (setq magit-git-debug (not magit-git-debug))
  )

(use-package gitconfig-mode)

(use-package git-gutter
  :pin melpa-stable
  :diminish git-gutter-mode
  :hook (emacs-startup . global-git-gutter-mode)
  :bind (("M-g p" . git-gutter:previous-hunk)
         ("M-g n" . git-gutter:next-hunk)
         ("M-g r" . git-gutter:revert-hunk)))

(use-package dired-k
  :hook (dired-initial-position . dired-k)
  :bind (:map dired-mode-map ("g" . dired-k))
  :custom
  (dired-k-human-readable t))
