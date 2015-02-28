;;; config/vc.el --- version control systems

;; TODO: wdired モードで `vc-rename-file' を 使いたい

;; Magit
(use-package magit
  :ensure t)

;; GitGutter
(use-package git-gutter
  :config (global-git-gutter-mode +1)
  :ensure t)
