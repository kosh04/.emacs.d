;;; config/vc-git --- version control systems

;; TODO:
;; - wdired モードで `vc-rename-file' を 使いたい
;; - shell-mode で利用する Git コマンドでは PAGER を無効化したい
;;   * git --no-pager $*
;;   * git config core.pager cat 

(setf (symbol-function 'git-grep) #'vc-git-grep)
(setf (getenv "GIT_PAGER") "cat")

(global-set-key (kbd "C-x v @") 'vc-git-grep)

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

(defun user::magit-mode-quit ()
  "Quit Magit mode. (Discard all buffers, and restore window-configuration)
URL `https://www.manueluberti.eu/emacs/2018/02/17/magit-bury-buffer/'"
  (interactive)
  (let ((buffers (magit-mode-get-buffers)))
    (magit-restore-window-configuration)
    (mapc #'kill-buffer buffers)
    (message "Magit has broken!")))

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
         ("C-x G" . magit-list-repositories)
         :map magit-mode-map
         ("&" . user::open-repository-url)
         ("@" . vc-git-grep)
         ("Q" . user::magit-mode-quit)
         :map magit-status-mode-map
         ("RET" . magit-diff-visit-file-other-window)
         :map magit-repolist-mode-map
         ("f" . user::magit-repolist-open-dired)
         :map magit-repolist-mode-map
         ("n" . next-line)
         ("p" . previous-line)
         )
  :hook (magit-diff-visit-file . view-mode)
  :custom
  (magit-repository-directories
   `((,user-emacs-directory . 0) ;; "~/.emacs.d" or "~/.config/emacs"
     (,(locate-user-emacs-file "site-lisp") . 1)
     ("~/Documents/GitHub/" . 1)
     ("~/Downloads/gitrepo/" . 1)
     (,(substitute-in-file-name "$GOPATH/src/github.com/kosh04/") . 1)))

  ;; 履歴をコミット時刻で表示 (デフォルトはコミット時期 e.g."3 days")
  ;;(magit-log-margin '(t "%F %T%z" magit-log-margin-width t 18))

  (magit-no-confirm '(stage-all-changes))
  (magit-diff-refine-hunk 'all)       ; 変更箇所を単語単位でハイライト

  :config
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

(use-package dired-git-info
  :after dired
  :custom (dgi-auto-hide-details-p t)
  :config
  (define-key dired-mode-map ")" 'dired-git-info-mode)
  ;;(add-hook 'dired-after-readin-hook 'dired-git-info-auto-enable)
  )

