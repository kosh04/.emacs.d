;;; config/TypeScript

;; TODO: typescript.el / typescript-mode.el の使いやすさの違いは？

(use-package typescript-mode
  :defer t
  :config
  ;; 補完/定義ジャンプ
  (use-package tide
    :config (add-hook 'typescript-mode-hook #'tide-setup))
  (add-hook 'typescript-mode-hook #'eldoc-mode))
