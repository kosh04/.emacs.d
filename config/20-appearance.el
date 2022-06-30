;;; config/appearance --- 表示と外観,テーマ

;(setq inhibit-startup-screen t)

(show-paren-mode +1)

;; since Emacs25
(if (fboundp 'horizontal-scroll-bar-mode)
    (horizontal-scroll-bar-mode -1))
;;(scroll-bar-mode -1)

;; Font
(let ((font "Sarasa Mono T J:pixelsize=14:weight=regular:slant=normal"))
  (when (and window-system (font-info font)) ; ? (display-graphic-p)
    ;; TODO: マシン毎にフォントサイズを調整したい
    ;; "Sarasa Term J-10.5" (pixelsize=14)
    ;; "更紗等幅ゴシック J"
    (create-fontset-from-ascii-font font nil "coding")
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
  )

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
;; (require 'hl-line)
(setq  hl-line-sticky-flag nil)
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

(use-package rainbow-mode
  :hook (emacs-lisp-mode))

;;; Theme
;(csetq frame-background-mode 'dark)
;; (load-theme 'zenburn)
;; (load-theme 'wombat)

'
(use-package kaolin-themes
  :init
  (load-theme 'kaolin-dark t))

(use-package modus-themes
  :hook (after-init . modus-themes-load-vivendi))
