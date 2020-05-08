;;; config/Tab,Workspace

;; tab, workspace, window-configuration manager

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
  '(progn
   (global-set-key [C-tab]   'tabbar-forward-group)
   (global-set-key [C-S-tab] 'tabbar-backward-group)
   )
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

;; new feature tabs (Emacs 27+)
(use-package tab-bar
  :ensure nil
  :if (version<= "27.0" emacs-version)
  :init (tab-bar-mode +1)
  :custom
  (tab-bar-tab-hints t)
  :bind (("C-x 6 ." . tab-next)
         ("C-x 6 ," . tab-previous)
         ("C-x 6 l" . tab-bar-list))
  :config
  (tab-rename "main")
  (require 'hi-lock)
  (set-face-attribute 'tab-bar-tab nil :inherit 'hi-green)
  )

(use-package tab-line
  :ensure nil
  :if (version<= "27.0" emacs-version)
  ;;:init (global-tab-line-mode +1)
  )

(use-package eyebrowse
  :demand t
  :config (eyebrowse-mode +1)
  :custom
  (eyebrowse-wrap-around t)
  ;;(eyebrowse-new-workspace t)
  :custom-face
  (eyebrowse-mode-line-active
   ((t (:inverse-video t))))
  )

;; ブックマークは新しいワークスペースで開いてみる
(with-eval-after-load 'bookmark
  (setq bookmark-after-jump-hook
        '(eyebrowse-create-window-config view-mode)))
