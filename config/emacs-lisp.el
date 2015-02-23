;;; config/emacs-lisp.el

(define-key emacs-lisp-mode-map (kbd "C-x C-r") 'eval-region)
(define-key lisp-interaction-mode-map (kbd "C-x C-r") 'eval-region)

(define-key emacs-lisp-mode-map (kbd "RET") 'newline-and-indent)
(define-key lisp-interaction-mode-map (kbd "RET") 'newline-and-indent)

;; Eldoc
(require 'eldoc)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
;; 引数表示をslime風にする
(set-face-background 'eldoc-highlight-function-argument "darkseagreen2")
(set-face-bold-p 'eldoc-highlight-function-argument nil)
(setq eldoc-idle-delay 0.2
      eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit)

;; (setq find-function-C-source-directory "~/src/emacs-22.2/src")
(define-key help-map (kbd "j") 'find-function) ; or #'find-variable
(define-key help-map (kbd "C-l") 'find-library)
;; ETAGS は再帰ができない？

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
  (put 'cl-flet 'common-lisp-indent-function (get 'flet 'common-lisp-indent-function))
  (put 'cl-labels 'common-lisp-indent-function (get 'labels 'common-lisp-indent-function))
  ;;(put 'cl-macrolet 'common-lisp-indent-function (get 'macrolet 'common-lisp-indent-function))
  t)

;; elisp 特有の関数のインデントには lisp-indent-function
;; cl-labels 等の複雑なインデントには common-lisp-indent-function
;; 両立が難しい
(setq lisp-indent-function #'lisp-indent-function)
;;(setq lisp-indent-function #'common-lisp-indent-function)
