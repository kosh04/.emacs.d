;;; config/tabbar.el

(require 'tabbar)

(tabbar-mode +1)

(global-set-key (kbd "C-x C-.")  'tabbar-forward-tab)
(global-set-key (kbd "C-x C-,") 'tabbar-backward-tab)

;; kill-buffer した後に戻るバッファが変わるのを抑える
(remove-hook 'kill-buffer-hook 'tabbar-buffer-track-killed)

(custom-set-variables
 '(tabbar-use-images t)
 '(tabbar-separator '(0.8))
 )

(setq
 tabbar-scroll-left-help-function 'ignore
 tabbar-scroll-right-help-function 'ignore
 tabbar-help-on-tab-function 'ignore
 tabbar-home-help-function 'ignore
 ;tabbar-buffer-home-button '(("[+]") "[-]")
 tabbar-scroll-left-button '(("") "")
 tabbar-scroll-right-button '(("") ""))

;; 外観の変更
(set-face-attribute 'tabbar-default nil
                    :family (face-attribute 'default :family)
                    :background (face-attribute 'mode-line-inactive :background)
                    :height 0.9)

(set-face-attribute 'tabbar-unselected nil
                    :background (face-attribute 'mode-line-inactive :background)
                    :foreground (face-attribute 'mode-line-inactive :foreground)
                    :box nil)

(set-face-attribute 'tabbar-selected nil
                    :background (face-attribute 'mode-line :background)
                    :foreground (face-attribute 'mode-line :foreground)
                    :box nil)
