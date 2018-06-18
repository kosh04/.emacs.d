;;; config/vc-git --- version control systems

;; TODO:
;; - wdired モードで `vc-rename-file' を 使いたい
;; - shell-mode で利用する Git コマンドでは PAGER を無効化したい
;;   * git --no-pager $*
;;   * git config core.pager cat 

(setf (symbol-function 'git-grep) #'vc-git-grep)
;;(setf (getenv "PAGER") "cat")

;; Magit
;; magit-auto-revert-mode によるプチフリーズに注意 (特に NTEmacs)
(use-package magit
  ;;:if (executable-find "git")
  :defer t
  :pin #:melpa-stable
  :bind ("C-x g" . magit-status)
  :config (add-to-list 'magit-no-confirm 'stage-all-changes))

(use-package git-gutter
  :pin melpa-stable
  :diminish git-gutter-mode
  :init (global-git-gutter-mode +1)
  :bind (("M-g p" . git-gutter:previous-hunk)
         ("M-g n" . git-gutter:next-hunk)
         ("M-g r" . git-gutter:revert-hunk)))

(use-package dired-k
  :defer t
  :init (add-hook 'dired-initial-position-hook 'dired-k)
  :bind (:map dired-mode-map ("g" . dired-k))
  :config
  (setq dired-k-human-readable t))
