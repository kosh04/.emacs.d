;;; config/appearance --- 表示と外観,テーマ

;(setq inhibit-startup-screen t)

(show-paren-mode +1)

;; since Emacs25
(if (fboundp 'horizontal-scroll-bar-mode)
    (horizontal-scroll-bar-mode -1))
;;(scroll-bar-mode -1)

;; Font
;; TODO: マシン毎にフォントサイズを調整したい
;; "Sarasa Term J-10.5" (pixelsize=14)
;; "更紗等幅ゴシック J"
(defvar user-font
  "Sarasa Term J:pixelsize=14:weight=regular:slant=normal")

(when window-system
  (if (not (font-info user-font))       ; ? (display-graphic-p)
      (warn "Not found font: %s" user-font)
    (create-fontset-from-ascii-font user-font nil "coding")
    (add-to-list 'default-frame-alist '(font . "fontset-coding"))))

;; Frame
(add-to-list 'default-frame-alist '(alpha . (0.90 0.90)))
;;(setf (frame-parameter nil 'alpha) '(0.90 0.90))
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; アクティブでないウィンドウのカーソルを表示/非表示
;;(setq-default cursor-in-non-selected-windows nil)

;; 行番号
;; `linum-mode' は `git-gutter' と被るため非推奨
(use-package display-line-numbers
  ;; NOTE: 特殊バッファに行番号は不要
  :hook ((prog-mode
          text-mode
          conf-mode)
         . display-line-numbers-mode)
  :custom
  (display-line-numbers-widen t))

;;(display-fill-column-indicator-mode +1)

(setq frame-title-format
      `(" %b " (buffer-file-name "(%f)") " on " system-name
        " - Emacs " emacs-version))

;; モードライン
(line-number-mode)
(column-number-mode)
(size-indication-mode)
;(display-battery-mode t)

;; (setq mode-line-compact t)

;; アクティブなウィンドウをモードラインの色で判別する
;; 端末タイプの詳細はinfo参照: (info "(elisp) Defining Faces")
;; - dark means "emacs -rv" or dark-theme (<-> light)
;; - tty means "emacs -nw" (<-> graphic)
(custom-set-faces
 '(mode-line
   ((((class color) (min-colors 88) (background light) (type graphic))
     :box (:line-width -1 :style released-button)
     :background "black"
     :foreground "gray95")
    (((class color) (background dark) (type graphic))
     :box (:line-width -1 :style released-button)
     :background "#6846A5"              ; 本紫
     :foreground "gray95")
    (((class color) (min-colors 256) (type tty))
     :background "#0067C0"              ; 青 (JIS)
     :foreground "gray95")
    (((class color) (min-colors 8) (type tty))
     :background "blue"
     :foreground "white")
    (t :inverse-video t))))

(use-package nyan-mode
  :if window-system
  :hook (emacs-startup . nyan-mode)
  :custom
  (nyan-bar-length 12))

;; フリンジ (fringes: 縁) に色々追加する
;; バッファ末尾
(setq-default indicate-empty-lines t)
;; バッファ境界
(setq-default indicate-buffer-boundaries 'right)

;; 行カーソル
(use-package hl-line
  :hook
  ((help-mode
    finder-mode
    occur-mode
    tabulated-list-mode)
   . hl-line-mode)
  :custom
  (hl-line-sticky-flag nil))

;; \C-x $ `set-selective-display'
;; 指定した桁数以上字下げしている行を隠す

;; Hide/Show (コメントを非表示にするマイナーモード)
(defalias 'hideshow-minor-mode #'hs-minor-mode)
(with-eval-after-load 'hideshow
  (add-hook 'hs-minor-mode-hook 'hs-hide-all))

(use-package rainbow-mode
  :hook (emacs-lisp-mode))

;; 隣接するウィンドウをマウスでドラッグできるバーの追加
(customize-set-variable 'window-divider-default-right-width 2)
(window-divider-mode +1)

;; 端末エミュレータは基本的にダークモードのみ
;; この設定がないと端末が黒背景であっても background-mode=light として扱われてしまう (なぜ？)
;; (setf (terminal-parameter nil 'background-mode) 'dark) ; daemon では効果なし
;; (setq frame-background-mode (if window-system nil 'dark))

;;; Theme

;;; Theme
;(csetq frame-background-mode 'dark)
;; (load-theme 'zenburn)
;; (load-theme 'wombat)

'
(use-package kaolin-themes
  :if window-system
  :init
  (load-theme 'kaolin-dark t))

(use-package modus-themes
  :if window-system
  ;; NOTE: built-in theme since emacs-30.0
  :demand
  :config
  (load-theme 'modus-vivendi t)
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t))

;; (use-package standard-themes
;;   :config (standard-themes-load-dark))
