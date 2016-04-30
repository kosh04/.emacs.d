;;; config/emacs-lisp

;; Emacs Source Repository
;; http://git.savannah.gnu.org/cgit/emacs.git

;; NOTE: lisp-interaction-mode は emacs-lisp-mode の子モードのため hook が継承される

(setq find-function-C-source-directory
      (if (file-directory-p #1="~/src/gitrepo/emacs/src/") #1#
          ;; 常に最新版を取得するため参照するソース元が一致しないかも
          ;; "http://git.savannah.gnu.org/cgit/emacs.git/plain/src/"
          "https://github.com/emacs-mirror/emacs/raw/emacs-24/src/"))

(add-to-list 'auto-mode-alist '("Cask" . emacs-lisp-mode))

;; (define-key emacs-lisp-mode-map (kbd "C-x C-r") 'eval-region)
;; (define-key lisp-interaction-mode-map (kbd "C-x C-r") 'eval-region)

(define-key emacs-lisp-mode-map (kbd "RET") 'newline-and-indent)
(define-key lisp-interaction-mode-map (kbd "RET") 'newline-and-indent)

;; Eldoc
(use-package eldoc
  :diminish eldoc-mode
  :config
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook 'eldoc-mode)
  ;; 引数表示をSLIME風にする
  (set-face-attribute 'eldoc-highlight-function-argument nil
                      :background "darkseagreen2"
                      :underline nil
                      :bold nil)
  (setq eldoc-idle-delay 0.2
        eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit))

;; elisp-slime-nav [M-.] [M-,]
(use-package elisp-slime-nav
  :diminish elisp-slime-nav-mode
  :config (add-hook 'emacs-lisp-mode-hook 'turn-on-elisp-slime-nav-mode))

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
(load "cl-indent")
(with-eval-after-load 'cl-indent
  (setf (get 'cl-flet 'common-lisp-indent-function)
        (get 'flet 'common-lisp-indent-function))
  (setf (get 'cl-labels 'common-lisp-indent-function)
        (get 'labels 'common-lisp-indent-function))
  (setf (get 'cl-macrolet 'common-lisp-indent-function)
        (get 'macrolet 'common-lisp-indent-function))
  t)

;; elisp 特有の関数のインデントには lisp-indent-function
;; cl-labels 等の複雑なインデントには common-lisp-indent-function
;; 両立が難しい
;;(setq lisp-indent-function #'lisp-indent-function)
;;(setq lisp-indent-function #'common-lisp-indent-function)

;; インデントを調整
(setf (get 'font-lock-add-keywords 'lisp-indent-function) 1)
(setf (get 'completing-read 'lisp-indent-function) 1)

(defun toggle-scratch-buffer (&optional other-window)
  "scratchバッファを表示する.
引数 OTHER-WINDOW を指定した場合はポップアップ表示する."
  (interactive "P")
  (let ((scratch (get-buffer-create "*scratch*")))
    (if other-window
        (pop-to-buffer scratch t)
      (switch-to-buffer scratch))
    (or (eq major-mode initial-major-mode)
        (funcall initial-major-mode))))

(global-set-key (kbd "C-x t s") 'toggle-scratch-buffer)

;; REPL
(global-set-key (kbd "C-x t l") 'ielm)

;; ParEdit
;; http://www.emacswiki.org/emacs/ParEdit
(use-package paredit
  :defer t
  :disabled t
  :config
  (progn
    (dolist (hook '(emacs-lisp-mode-hook
                    ;;lisp-interaction-mode-hook
                    lisp-mode-hook
                    ielm-mode-hook
                    scheme-mode-hook))
      (add-hook hook 'enable-paredit-mode))
    (require 'eldoc)
    (eldoc-add-command 'paredit-backward-delete 'paredit-close-round)))

;; interactive macroexpand
(use-package macrostep
  :defer t
  :bind ("C-c RET" . macrostep-expand)
  :ensure t)

;; prettify-symbol
;; Emacs24.4 以降で利用可能
(defun user:prettify-lambda ()
  (setq prettify-symbols-alist
        '(("lambda" . "\u03BB")         ; λ
          ("/=" . "\u2260")             ; ≠
          ("<=" . "\u2264")             ; ≤
          (">=" . "\u2267")             ; ≧
          ))
  t)

(add-hook 'emacs-lisp-mode-hook 'user:prettify-lambda)

;; less is more
(use-package nameless
  :config
  (custom-set-variables
   '(nameless-global-aliases nil))
  (add-hook 'emacs-lisp-mode-hook #'nameless-mode))
