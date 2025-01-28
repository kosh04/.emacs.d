;;; config/vc-git --- version control systems

;; TODO:
;; - wdired モードで `vc-rename-file' を 使いたい
;; - shell-mode で利用する Git コマンドでは PAGER を無効化したい
;;   * git --no-pager $*
;;   * git config core.pager cat
;; - Run term+tig

(setf (symbol-function 'git-grep) #'vc-git-grep)
(setf (getenv "GIT_PAGER") "cat")

(global-set-key (kbd "C-x v @") 'vc-git-grep)

(define-advice vc-git-grep (:after (&rest _) window-select)
  "*grep* ウィンドウにフォーカスする."
  (pop-to-buffer next-error-last-buffer))

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
;;(defalias 'user::open-repository-url 'git-link-homepage)

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
  "Quit Magit mode.(Discard all buffers, and restore window-configuration)
URL `https://www.manueluberti.eu/emacs/2018/02/17/magit-bury-buffer/'"
  (interactive)
  (let ((buffers (magit-mode-get-buffers)))
    (magit-restore-window-configuration)
    (mapc #'kill-buffer buffers)
    (message "Magit has broken!")))
;; ?>
;; (setopt magit-bury-buffer-function #'magit-restore-window-configuration)

(defun user::dired-root-dir ()
  (interactive)
  ;; (locate-dominating-file default-directory ".git")
  (dired (vc-root-dir)))

;; Magit
;; magit-auto-revert-mode によるプチフリーズに注意 (特に NTEmacs)
(use-package magit
  ;;:if (executable-find "git")
  :pin #:melpa-stable
  :bind (;("C-x g" . magit-status)
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
         ;; コミット間 diff の URL を生成したい
         ;; :map magit-diff-mode-map
         )
  :hook
  (magit-diff-visit-file . view-mode)
  (after-save . magit-after-save-refresh-status)
  :custom
  (magit-repository-directories
   `((,user-emacs-directory . 0) ;; "~/.emacs.d" or "~/.config/emacs"
     (,(locate-user-emacs-file "site-lisp") . 1)
     ("~/Documents/GitHub/" . 1)
     ("~/Downloads/gitrepo/" . 1)
     (,(substitute-in-file-name "$GOPATH/src/github.com/kosh04/") . 1)))

  ;; 履歴をコミット時刻で表示 (デフォルトはコミット時期 e.g."3 days")
  ;;(magit-log-margin '(t "%F %T%z" magit-log-margin-width t 18))
  (magit-refresh-status-buffer nil)
  (magit-no-confirm '(stage-all-changes))
  (magit-diff-refine-hunk 'all)       ; 変更箇所を単語単位でハイライト
  (magit-status-goto-file-position t)

  ;; *Magit* バッファの表示方法どうする？
  ;;(magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)

  :config
  ;; see [$] `magit-process'
  ;; (setq magit-git-debug (not magit-git-debug))
  )


(use-package transient
  :custom
  ;; original: transient/history.el
  (transient-history-file
   (locate-user-emacs-file "cache/transient/history.el"))
  (transient-levels-file
   (locate-user-emacs-file "cache/transient/levels.el"))
  (transient-values-file
   (locate-user-emacs-file "cache/transient/values.el")))

;; Contains of Magit?
;(use-package gitconfig-mode)

(use-package git-gutter
  ;; NOTE: linum.el (obsolete) をサポートしているため、メンテナンス頻度少なめ
  :diminish git-gutter-mode
  :hook (emacs-startup . global-git-gutter-mode)
  :bind (("M-g p" . git-gutter:previous-hunk)
         ("M-g n" . git-gutter:next-hunk)
         ("M-g r" . git-gutter:revert-hunk)))

;; TODO: diff-hl の使い心地はどう？

;; FIXME: ファイル数の多いディレクトリを考慮して、起動時は無効にしたほうが無難？
(use-package dired-k
  :hook (dired-initial-position . dired-k)
  :bind (:map dired-mode-map ("g" . dired-k))
  :custom
  (dired-k-style 'git)
  (dired-k-human-readable t))

;; 直近のコミットメッセージと日付情報を表示する
(use-package dired-git-info
  ;;:hook (dired-after-readin . dired-git-info-auto-enable)
  :custom (dgi-auto-hide-details-p t)
  :bind (:map dired-mode-map (")" . dired-git-info-mode))
  )

(use-package blamer
  :disabled
  :config
  (global-blamer-mode +1))

;; ブラウザリンク生成 (主に init.el 読書会用)
(use-package git-link
  :bind (("C-c =" . git-link))
  :custom
  (git-link-use-commit t))
