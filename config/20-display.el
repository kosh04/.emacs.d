;;; config/display --- 表示関係

;(setq inhibit-startup-screen t)

;; 24.3.1@darwin にて未定義
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(menu-bar-mode)
(show-paren-mode)

;; since Emacs25
(if (fboundp 'horizontal-scroll-bar-mode)
    (horizontal-scroll-bar-mode -1))

;; Frame
(add-to-list 'default-frame-alist '(alpha . (0.95 0.95)))
;;(setf (frame-parameter nil 'alpha) '(0.95 0.95))

;; アクティブでないウィンドウのカーソルを表示/非表示
;;(setq-default cursor-in-non-selected-windows nil)

;; git-gutter と被る
;;(global-linum-mode)

(setq frame-title-format
      `(" %b " (buffer-file-name "(%f)") " on " ,(system-name)
        " - " ,(format "Emacs %s" emacs-version)))

;; モードライン
(line-number-mode)
(column-number-mode)
(size-indication-mode)
;(display-battery-mode t)

;; アクティブなウィンドウをモードラインの色で判別する
(set-face-background 'mode-line "black")
(set-face-foreground 'mode-line "gray95")
;;(invert-face 'mode-line)

(use-package nyan-mode
  :if window-system
  :hook (emacs-startup . nyan-mode)
  :custom
  (nyan-bar-length 12))

;; バッファ末尾の可視化 (fringe)
;; (set-fringe-mode 5)
(setq-default indicate-empty-lines t)
(set-face-background 'fringe "gray80")

;; 行カーソル
(require 'hl-line)
(set-face-background 'hl-line "#DEEDFF")
(add-hook 'help-mode-hook 'hl-line-mode)

(add-hook 'tabulated-list-mode-hook 'hl-line-mode)
(add-hook 'finder-mode-hook 'hl-line-mode)
(add-hook 'occur-mode-hook 'hl-line-mode)

;; \C-x $ `set-selective-display'
;; 指定した桁数以上字下げしている行を隠す

;; Hide/Show (コメントを非表示にするマイナーモード)
(defalias 'hideshow-minor-mode #'hs-minor-mode)
(with-eval-after-load 'hideshow
  (add-hook 'hs-minor-mode-hook 'hs-hide-all))
