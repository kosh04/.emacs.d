;;; config/pkg-tabbar.el

(require 'tabbar)
(tabbar-mode)

(global-set-key (kbd "C-x C-.")  'tabbar-forward-tab)
(global-set-key (kbd "C-x C-,") 'tabbar-backward-tab)

;; kill-buffer した後に戻るバッファが変わるのを抑える
(remove-hook 'kill-buffer-hook 'tabbar-buffer-track-killed)

;; TODO: 外観の調整
(when nil
(set-face-attribute 'tabbar-default nil :background "gray60")
(set-face-attribute 'tabbar-selected nil :background "#f2f2f6" :foreground "black" :box nil)
(set-face-attribute 'tabbar-unselected nil :background "gray85" :foreground "gray30" :box nil)
(set-face-attribute 'tabbar-button nil :box '(:line-width 1 :color "gray72" :style released-button))
;(set-face-attribute 'tabbar-separator nil :height 0.7)
)

