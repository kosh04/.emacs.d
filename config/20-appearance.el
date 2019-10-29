;;; config/appearance --- 表示と外観

;(setq inhibit-startup-screen t)

;; 24.3.1@darwin にて未定義
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(menu-bar-mode)
(show-paren-mode)

;; since Emacs25
(if (fboundp 'horizontal-scroll-bar-mode)
    (horizontal-scroll-bar-mode -1))
;;(scroll-bar-mode -1)

;; Font
(when (and window-system (font-info "Sarasa Term J"))
  ;; TODO: マシン毎にフォントサイズを調整したい
  ;; "Sarasa Term J:pixelsize=16"
  ;; "Sarasa Term J-10.5"
  ;; "Sarasa Term J:pixelsize=16:weight=regular:slant=normal"
  ;; "更紗等幅ゴシック J"
  (create-fontset-from-ascii-font "Sarasa Term J" nil "coding")
  (add-to-list 'default-frame-alist '(font . "fontset-coding")))

;; Frame
(add-to-list 'default-frame-alist '(alpha . (0.90 0.90)))
;;(setf (frame-parameter nil 'alpha) '(0.90 0.90))
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; アクティブでないウィンドウのカーソルを表示/非表示
;;(setq-default cursor-in-non-selected-windows nil)

;; git-gutter と被る
;; -> `global-display-line-numbers-mode'
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
;; 端末タイプの詳細はinfo参照: (info "(elisp) Defining Faces")
(custom-set-faces
 '(mode-line
   ((((class color) (min-colors 88) (background light) (type graphic))
     :box (:line-width -1 :style released-button)
     :background "black"
     :foreground "gray95")
    (((class color) (background dark) (type graphic)) ; dark means "emacs -rv" or dark-theme
     :box (:line-width -1 :style released-button)
     :background "#6846A5"              ; 本紫
     :foreground "gray95")
    (((class color) (min-colors 16777216) (type tty)) ; tty means "emacs -nw"
     :background "#0067C0"              ; 青 (JIS)
     :foreground "gray95")
    (((class color) (min-colors 256) (type tty))
     :background "blue"
     :foreground "grey80")
    (t :inverse-video t))))

(use-package nyan-mode
  :if window-system
  :hook (emacs-startup . nyan-mode)
  :custom
  (nyan-bar-length 12))

;; バッファ末尾の可視化 (fringe)
;; (set-fringe-mode 5)
(setq-default indicate-empty-lines t)

;; 行カーソル
;; (require 'hl-line)
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
