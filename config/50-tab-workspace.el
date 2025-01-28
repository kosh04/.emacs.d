;;; config/Tab,Workspace

;; Tab, Workspace, Window-Configuration Manager

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
  :if (version<= "27.0" emacs-version)
  :demand
  :custom
  (tab-bar-show t)
  (tab-bar-close-button-show 'selected)
  (tab-bar-auto-width nil)		; 勝手に伸び縮みしないで
  (tab-bar-history-mode t)
  (tab-bar-new-tab-to 'rightmost)
  (tab-bar-tab-hints t)
  (tab-bar-select-tab-modifiers [meta]) ; alt-[number]:switchToTab
  (tab-bar-new-tab-choice "*scratch*")
  :bind (("C-x t l" . tab-list))
  :config
  ;;(set-face-attribute 'tab-bar-tab nil :inherit 'highlight)
  (tab-rename "main" 1)
  ;; TODO: C-x t k でタブと関連バッファをまとめて削除したい
  )

(use-package tab-line
  :disabled
  :if (version<= "27.0" emacs-version)
  :demand
  :custom
  (tab-line-close-button-show 'selected)
  :config
  (global-tab-line-mode +1)
  )

;; パンくずリスト
;; which-function-mode の代替に使える？
(use-package breadcrumb
  :hook after-init)

(use-package eyebrowse
  :disabled
  :demand
  :config (eyebrowse-mode +1)
  :custom
  (eyebrowse-wrap-around t)
  ;;(eyebrowse-new-workspace t)
  :custom-face
  (eyebrowse-mode-line-active
   ((t (:inverse-video t))))
  )

;; ブックマークは新しいワークスペースで開きたい
;; -> C-x x t (other-tab-prefix) C-x r b (bookmark-jump)
(with-eval-after-load 'bookmark
  ;; (add-hook 'bookmark-after-jump-hook 'tab-new)
  ;; (add-hook 'bookmark-after-jump-hook 'eyebrowse-create-window-config)
  )

;; タブの見た目を強化
(use-package centaur-tabs
  :disabled
  :demand
  :init
  ;;(setq centaur-tabs-style "alternate") ; ?
  (setq centaur-tabs-set-icons t)
  :config
  (centaur-tabs-mode +1))
