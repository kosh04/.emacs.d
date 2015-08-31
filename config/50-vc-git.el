;;; config/vc-git.el --- version control systems

;; TODO: wdired モードで `vc-rename-file' を 使いたい

;; Magit
;; magit-auto-revert-mode によるプチフリーズに注意 (特に NTEmacs)
(use-package magit
  :defer t
  :bind ("C-x g" . magit-status)
  :config (add-to-list 'magit-no-confirm 'stage-all-changes)
  :ensure t)

(use-package git-gutter
  :defer t
  :diminish git-gutter-mode
  :bind (("M-g p" . git-gutter:previous-hunk)
         ("M-g n" . git-gutter:next-hunk)
         ("M-g r" . git-gutter:revert-hunk))
  :config (global-git-gutter-mode +1)
  :ensure t)

(use-package dired-k
  :defer t
  :init (add-hook 'dired-initial-position-hook 'dired-k) 
  :config (define-key dired-mode-map (kbd "g") 'dired-k))
