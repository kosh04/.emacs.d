;;; config/emacs-lisp

;; NOTE: lisp-interaction-mode は emacs-lisp-mode の子モードのため hook が継承される

;; NOTE: Emacs 28.1 より、同等の機能を持つ `lisp-data-mode' が定義される予定.
(define-derived-mode user::emacs-lisp-dataset-mode
  emacs-lisp-mode "elisp-dataset"
  ;; surpress flycheck linter
  (setq-local no-byte-compile t))

(unless (fboundp 'lisp-data-mode)
  (defalias 'lisp-data-mode 'user::emacs-lisp-dataset-mode))

(add-to-list 'auto-mode-alist
 (cons (rx (or "Cask" "abbrev_defs" "recentf" "bookmarks") eos)
       'lisp-data-mode))

;; (define-key emacs-lisp-mode-map (kbd "C-x C-r") 'eval-region)
;; (define-key lisp-interaction-mode-map (kbd "C-x C-r") 'eval-region)

;; (define-key emacs-lisp-mode-map (kbd "RET") 'newline-and-indent)
;; (define-key lisp-interaction-mode-map (kbd "RET") 'newline-and-indent)

(add-hook 'emacs-lisp-mode 'auto-fill-mode)

(setq load-prefer-newer t)

;; 暗号化ファイルを (load FILENAME) で読み込めるように
(add-to-list 'load-suffixes ".el.gpg")

;; Eldoc
(use-package eldoc
  :diminish eldoc-mode
  :custom
  (eldoc-idle-delay 0.3)
  (eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit)
  :custom-face
  ;; 引数表示をSLIME風にする
  (eldoc-highlight-function-argument
   ((t (:inherit highlight)))))

;; elisp-slime-nav [M-.] [M-,]
(use-package elisp-slime-nav
  ;; or :if (version< emacs-version "25.1")
  :unless (fboundp 'xref-find-definitions)
  :diminish elisp-slime-nav-mode
  :config
  (add-hook 'emacs-lisp-mode-hook 'turn-on-elisp-slime-nav-mode)
  (add-hook 'ielm-mode-hook 'turn-on-elisp-slime-nav-mode))

(define-key help-map (kbd "j") 'find-function) ; or #'find-variable
(define-key help-map (kbd "C-l") 'find-library)
;;(find-function-setup-keys)

(defun recompile-and-load-file ()
  "*.lc があったら再コンパイルとロードを行う."
  (let ((file (buffer-file-name)))
    (when (and file
               (member major-mode '(emacs-lisp-mode lisp-interaction-mode))
               (file-exists-p (byte-compile-dest-file file))
               (null (check-parens)))
      (let ((feature (intern (file-name-sans-extension (file-name-nondirectory file)))))
        (byte-compile-file file (featurep feature))))))

;; (add-hook 'after-save-hook 'recompile-and-load-file)
;; (remove-hook 'after-save-hook 'recompile-and-load-file)

;; インデント
;;(load "cl-indent")
(with-eval-after-load 'cl-indent
  (setf (get 'cl-flet 'common-lisp-indent-function)
        (get 'flet 'common-lisp-indent-function))
  (setf (get 'cl-labels 'common-lisp-indent-function)
        (get 'labels 'common-lisp-indent-function))
  (setf (get 'cl-macrolet 'common-lisp-indent-function)
        (get 'macrolet 'common-lisp-indent-function)))

;; elisp 特有の関数のインデントには lisp-indent-function
;; cl-labels 等の複雑なインデントには common-lisp-indent-function
;; 両立が難しい
;;(setq lisp-indent-function #'lisp-indent-function)
;;(setq lisp-indent-function #'common-lisp-indent-function)

;; インデントを調整
(setf (get 'font-lock-add-keywords 'lisp-indent-function) 1)
(setf (get 'completing-read 'lisp-indent-function) 1)

(defun user::toggle-scratch-buffer (&optional other-window)
  "scratchバッファを表示する.
引数 OTHER-WINDOW を指定した場合はポップアップ表示する."
  (interactive "P")
  (let ((scratch (get-buffer-create "*scratch*")))
    (if other-window
        (pop-to-buffer scratch t)
      (switch-to-buffer scratch))
    (or (eq major-mode initial-major-mode)
        (funcall initial-major-mode))))

;; ParEdit
;; http://www.emacswiki.org/emacs/ParEdit
(use-package paredit
  :disabled
  :config
  (dolist (hook '(emacs-lisp-mode-hook
                  ;;lisp-interaction-mode-hook
                  lisp-mode-hook
                  ielm-mode-hook
                  scheme-mode-hook))
    (add-hook hook 'enable-paredit-mode))
  (require 'eldoc)
  (eldoc-add-command 'paredit-backward-delete 'paredit-close-round))

;; Debug

;; debug.el (interpreter debugger)
;; (info "(elisp) Debugger")
;; 主な用途は無限ループしている関数のデバッグ
;; - d `debugger-step-through' ステップ実行
;; - c `debugger-continue' Continue
;; - b `debugger-frame' Request entry to debugger when this frame exits.
;; - u `debugger-frame-clear'
;; - e `debugger-eval-expression'
;; - q `top-level'
;; - r `debugger-return-value'
;; - h `describe-mode'
;; - j `debugger-jump'
;; - l `debugger-list-functions'
;; - v `debugger-toggle-locals'
;; - R `debugger-record-expression' ?

(with-eval-after-load 'debug
  (add-hook 'debugger-mode-hook #'hl-line-mode))

;; Edebug (source level debugger)
;; (info "(elisp) Edebug")
;; see Edebug menu commands
;; - SPC `edebug-step-mode'
;; - g `edebug-go-mode' Continue until break
;; - c `edebug-continue-mode' Continue until C-g
;; - d `edebug-backtrace' Display *Backtrace*
;; - n `edebug-next-mode'
;; - t `edebug-trace-mode' Step + Wait 1sec?
;; - h `edebug-goto-here' Jump untill cursor position
;; - f `edebug-forward-sexp'
;; - o `edebug-step-out'
;; - i `edebug-step-in'
;; - C-] `abort-recursive-edit'
;; - b `edebug-set-breakpoint'
;; - u `edebug-unset-breakpoint'
;; - x `edebug-set-conditional-breakpoint' 条件付きbreak
;; - B `edebug-next-breakpoint' Show next breakpoint
;; - w `edebug-where' Restore cursor position
;; - e `edebug-eval-expression'
;; - E `edebug-visit-eval-list' visit *scratch* like debug buffer

(with-eval-after-load 'edebug
  ;; FIXME: view-mode-map が優先されるのはバグ？ (type e `view-exit')
  (defun edebug-mode--disable-view-mode ()
    (when view-mode
      (warn "`view-mode' temporally disabled in Edebug-mode")
      (view-mode -1)))
  ;;(add-hook 'edebug-mode-hook 'edebug-mode--disable-view-mode)
  (add-hook 'edebug-setup-hook 'edebug-mode--disable-view-mode)
  ;;(setq edebug-trace t)
  )

;; interactive macroexpand
(use-package macrostep
  :bind ("C-c RET" . macrostep-expand))

;; prettify-symbol
;; Emacs24.4 以降で利用可能
(custom-set-variables
 '(prettify-symbols-unprettify-at-point t))

(defun user:prettify-lambda ()
  (setq prettify-symbols-alist
        '(("lambda" . "\u03BB")         ; λ
          ("/=" . "\u2260")             ; ≠
          ("!=" . "\u2260")
          ("<=" . "\u2264")             ; ≤
          (">=" . "\u2267")             ; ≧
          ))
  t)

(add-hook 'emacs-lisp-mode-hook 'user:prettify-lambda)

(use-package dash
  :pin #:melpa-stable
  ;;:custom (dash-enable-fontlock t)
  :config
  (dash-enable-font-lock)
  )

;; font-lock や eval-last-sexp を names 開発用に拡張
;; `defun*' が有効で `cl-defun' が無効なのはどういう意図？
;; (defalias 'names--convert-cl-defun 'names--convert-defun)
(with-eval-after-load 'names
  (require 'names-dev))

;; less is more
(use-package nameless
  :custom
  (nameless-global-aliases nil)
  (nameless-private-prefix t)
  :hook (emacs-lisp-mode . nameless-mode))

;; Surpress byte-compile warning
;; (unless (fboundp 'elisp--preceding-sexp)
;;   (defalias 'elisp--preceding-sexp 'preceding-sexp))

(use-package erefactor
  ;; XXX: 開発用ファイル群のみ有効にしたい
  :config
  (defalias 'erefactor-prefix erefactor-map)
  (define-key emacs-lisp-mode-map "\C-c\C-v" 'erefactor-prefix)
  (define-key erefactor-map "@" 'erefactor-highlight-mode))

;; 主に init.el 読書会用
(add-to-list 'safe-local-variable-values '(erefactor-highlight-mode . nil))
(add-to-list 'safe-local-variable-values '(display-line-numbers . t))

(use-package flycheck-package
  :after flycheck
  :config (flycheck-package-setup))

(use-package delight
  ;; NOTE: Emacs28 ではモードラインに lexical/dynamic 情報が追加されるが delight と併用不可
  :if (version< emacs-version "28.0")
  :demand
  :config
  (delight 'lisp-interaction-mode "Eλ»")    ; *scratch*
  (delight 'inferior-emacs-lisp-mode "Eλ»") ; *ielm*
  (delight 'emacs-lisp-mode "Eλ")
  (delight 'lisp-mode "λ")
  )

;; nadvice でパッチを作るよりも便利なのだろうか？
(use-package el-patch :disabled)
