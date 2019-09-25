;;; config/vc-git --- version control systems

;; TODO:
;; - wdired モードで `vc-rename-file' を 使いたい
;; - shell-mode で利用する Git コマンドでは PAGER を無効化したい
;;   * git --no-pager $*
;;   * git config core.pager cat 

(setf (symbol-function 'git-grep) #'vc-git-grep)
(setf (getenv "GIT_PAGER") "cat")

(defun user::open-repository-url (&optional action)
  "現在開いているgitリポジトリのリモートURLを表示します."
  (interactive)
  (unless action
    (setq action #'browse-url))
  (let ((url (string-trim-right
              (with-output-to-string
                (call-process "git" nil standard-output nil "ls-remote" "--get-url")))))
    ;; or (setq url (magit-get "remote" "origin" "url"))
    (message "%s" url)
    ;; SSH Protocol -> HTTPS
    ;; NOTE: .wiki は GitHub Wiki のこと
    (when (string-match "git@\\(.+\\):\\(.+?\\)\\(\\.wiki\\)?\\(?:\\.git\\)?\\'" url)
      (setq url (format "https://%s/%s%s"
                        (match-string 1 url)
                        (match-string 2 url)
                        (if (match-string 3) "/wiki" ""))))
    (funcall action url)))

;; TODO: Tig のように u キーひとつで stage/unstage を切り替えたい
(defun user::magit-update-status ()
  "Stage/unstage file changes."
  (interactive)
  (pcase (magit-diff-type)
    (`untracked (magit-stage))
    (`unstaged (magit-stage))
    (`staged   (magit-unstage))))

(defun user::magit-repolist-open-dired (dir)
  "Open `DIR' repository."
  (interactive (list (tabulated-list-get-id)))
  (dired dir))

(defun magit-repolist-column-version--patch (f &rest args)
  "コミットのないリポジトリに対処するパッチ. (magit-2.91.0 にて修正予定)
See URL `https://github.com/magit/magit/issues/3686'"
  (condition-case err
      (apply f args)
    (wrong-type-argument ;; err=(wrong-type-argument stringp nil)
     "(gone)")
    (error (apply 'signal err))))

;; Magit
;; magit-auto-revert-mode によるプチフリーズに注意 (特に NTEmacs)
(use-package magit
  ;;:if (executable-find "git")
  :pin #:melpa-stable
  :bind (("C-x g" . magit-status)
         ("C-x t g" . magit-list-repositories)
         :map magit-mode-map
         ("&" . user::open-repository-url)
         :map magit-repolist-mode-map
         ("f" . user::magit-repolist-open-dired)
         )
  :custom
  (magit-repository-directories
   `((,user-emacs-directory . 0) ;; "~/.emacs.d" or "~/.config/emacs"
     (,(locate-user-emacs-file "site-lisp") . 1)
     ("~/Documents/GitHub/" . 1)
     ("~/Downloads/gitrepo/" . 1)
     (,(substitute-in-file-name "$GOPATH/src/github.com/kosh04/") . 1)))
  :config
  (add-to-list 'magit-no-confirm 'stage-all-changes)
  ;; see [$] `magit-process'
  ;; (setq magit-git-debug (not magit-git-debug))

  (let ((patch #'magit-repolist-column-version--patch))
    (if (version<= "2.91.0" (magit-version))
        (warn "The patch `%s' is no longer required: %s" patch #$)
      (advice-add 'magit-repolist-column-version :around patch)))
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
