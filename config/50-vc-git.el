;;; config/vc-git.el --- version control systems

;; TODO: wdired モードで `vc-rename-file' を 使いたい

;; Magit
(use-package magit
  :defer t
  :ensure t)

(use-package git-gutter
  :diminish git-gutter-mode
  :config (global-git-gutter-mode +1)
  :ensure t)
