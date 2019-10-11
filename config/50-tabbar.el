;;; config/tabbar

;; new feature tabs (emacs-27+)
(use-package tab-bar
  :ensure nil
  :if (version<= "27.0" emacs-version)
  :init (tab-bar-mode +1)
  :bind (("C-x 6 l" . tab-bar-list)
         ("C-x 6 b" . tab-bar-select-tab)
         ;;("C-x 6 o" . tab-bar-switch-to-next-tab)
         ;;("C-x 6 p" . tab-bar-switch-to-prev-tab)
         ("C-x C-." . tab-next)
         ("C-x C-," . tab-previous))
  :config
  (set-face-attribute 'tab-bar nil :height 1.0)
  ;;(set-face-attribute 'tab-bar-tab nil :inherit 'header-line)
  (set-face-attribute 'tab-bar-tab nil :inherit 'success)
  (set-face-attribute 'tab-bar-tab-inactive nil :foreground (face-foreground 'default) :background (face-background 'default) :inverse-video t)
  )

(use-package tab-line
  :ensure nil
  :if (version<= "27.0" emacs-version)
  :init (global-tab-line-mode +1)
  :config
  (set-face-attribute 'tab-line-tab nil :inherit 'warning)
  ;;(set-face-attribute 'tab-line-tab nil :background "#00F" :foreground (face-background 'highlight))
  (set-face-attribute 'tab-line-tab-inactive nil :foreground (face-foreground 'default) :background (face-background 'default) :inverse-video t)
  )

(use-package tabbar
  :if (not (version<= "27.0" emacs-version))
  :hook
  (emacs-startup . tabbar-mode)

  :custom
  (tabbar-use-images t)
  (tabbar-separator '(0.8))

  :config
  (tabbar-mwheel-mode -1)
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
