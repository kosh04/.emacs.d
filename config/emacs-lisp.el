;;; config/emacs-lisp.el

(global-set-key (kbd "C-x C-r") 'eval-region)

;; Eldoc
(require 'eldoc)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
;; 引数表示をslime風にする
(set-face-background 'eldoc-highlight-function-argument "darkseagreen2")
(set-face-bold-p 'eldoc-highlight-function-argument nil)
(setq eldoc-idle-delay 0.2
      eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit)

;; ETAGS は標準で再帰ができない？
;; (setq find-function-C-source-directory "~/src/emacs-22.2/src")
(define-key help-map (kbd "C-l") 'find-library)
(define-key help-map (kbd "j") 'find-function) ; or #'find-variable

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

