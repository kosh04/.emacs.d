;;; config/emoji

;; 豆腐文字を画像で代用する
(use-package emojify
  :config (add-hook 'after-init-hook #'global-emojify-mode))
