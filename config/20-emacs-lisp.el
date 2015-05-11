;;; config/emacs-lisp.el

;; `git clone https://github.com/emacs-mirror/emacs ~/src/gitrepo/emacs`
(setq find-function-C-source-directory "~/src/gitrepo/emacs/src/")

;; (define-key emacs-lisp-mode-map (kbd "C-x C-r") 'eval-region)
;; (define-key lisp-interaction-mode-map (kbd "C-x C-r") 'eval-region)

(define-key emacs-lisp-mode-map (kbd "RET") 'newline-and-indent)
(define-key lisp-interaction-mode-map (kbd "RET") 'newline-and-indent)

;; Eldoc
(use-package eldoc
  :diminish eldoc-mode
  :config
  (progn
    (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
    (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
    ;; 引数表示をSLIME風にする
    (set-face-attribute 'eldoc-highlight-function-argument nil
                        :background "darkseagreen2"
                        :underline nil
                        :bold nil)
    (setq eldoc-idle-delay 0.2
          eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit)))

;; elisp-slime-nav [M-.] [M-,]
(use-package elisp-slime-nav
  :diminish elisp-slime-nav-mode
  :config
  (progn
    (add-hook 'emacs-lisp-mode-hook 'turn-on-elisp-slime-nav-mode)
    (add-hook 'lisp-interaction-mode-hook 'turn-on-elisp-slime-nav-mode))
  :ensure t)

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

;; ParEdit
;; http://www.emacswiki.org/emacs/ParEdit
(use-package paredit
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
    (eldoc-add-command 'paredit-backward-delete 'paredit-close-round)
    ))
