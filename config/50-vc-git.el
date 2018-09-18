;;; config/vc-git --- version control systems

;; TODO:
;; - wdired モードで `vc-rename-file' を 使いたい
;; - shell-mode で利用する Git コマンドでは PAGER を無効化したい
;;   * git --no-pager $*
;;   * git config core.pager cat 

(setf (symbol-function 'git-grep) #'vc-git-grep)
;;(setf (getenv "PAGER") "cat")

(defun user:browse-repository-url (&optional action)
  "現在開いているgitリポジトリのリモートURLを表示します."
  (interactive)
  (unless action
    (setq action #'browse-url))
  (let ((url (with-temp-buffer
               (vc-git--call t "ls-remote" "--get-url")
               (s-trim (buffer-string)))))
    (message "%s" url)
    ;; git protocol -> https
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
  :bind ("C-x g" . magit-status)
  :config
  (add-to-list 'magit-no-confirm 'stage-all-changes)
  (bind-key "&" #'user:browse-repository-url magit-mode-map)
  ;; see [$] `magit-process'
  ;; (setq magit-git-debug (not magit-git-debug))
  )

(use-package git-gutter
  :pin melpa-stable
  :diminish git-gutter-mode
  :config (global-git-gutter-mode +1)
  :bind (("M-g p" . git-gutter:previous-hunk)
         ("M-g n" . git-gutter:next-hunk)
         ("M-g r" . git-gutter:revert-hunk))
  :ensure t)

(use-package dired-k
  :defer t
  :init (add-hook 'dired-initial-position-hook 'dired-k)
  :bind (:map dired-mode-map ("g" . dired-k))
  :config
  (setq dired-k-human-readable t))
