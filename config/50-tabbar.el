;;; config/tabbar

(use-package tabbar
  :hook
  (emacs-startup . tabbar-mode)

  :custom
  (tabbar-use-images t)
  (tabbar-separator '(0.8))

  :config
  (global-set-key (kbd "C-x C-.") 'tabbar-forward-tab)
  (global-set-key (kbd "C-x C-,") 'tabbar-backward-tab)
  (global-set-key [C-tab]   'tabbar-forward-group)
  (global-set-key [C-S-tab] 'tabbar-backward-group)

  ;; kill-buffer した後に戻るバッファが変わるのを抑える
  (add-hook 'tabbar-init-hook
            (lambda ()
              (remove-hook 'kill-buffer-hook 'tabbar-buffer-track-killed))
            'append)

  (setq
   tabbar-scroll-left-help-function 'ignore
   tabbar-scroll-right-help-function 'ignore
   tabbar-help-on-tab-function 'ignore
   tabbar-home-help-function 'ignore
   ;;tabbar-buffer-home-button '(("[+]") "[-]")
   tabbar-scroll-left-button '(("") "")
   tabbar-scroll-right-button '(("") ""))

  ;; 外観の変更 (フラットな感じで)
  ;; TODO :custom-face
  (set-face-attribute 'tabbar-default nil
                      :family (face-attribute 'default :family)
                      :background (face-attribute 'mode-line-inactive :background)
                      :height 0.8)

  (set-face-attribute 'tabbar-unselected nil
                      :background (face-attribute 'mode-line-inactive :background)
                      :foreground (face-attribute 'mode-line-inactive :foreground)
                      :box '(:line-width -1 :style released-button))

  (set-face-attribute 'tabbar-selected nil
                      :background (face-attribute 'mode-line :background)
                      :foreground (face-attribute 'mode-line :foreground)
                      :box '(:line-width -1 :style released-button))
  )
