;;; config/CommonLisp

(use-package slime
  ;;:disabled
  :pin #:melpa-stable
  :config
  (slime-setup '(slime-repl slime-fancy))
  ;; https://common-lisp.net/project/slime/doc/html/Multiple-Lisps.html
  ;; C-u - M-x slime
  (setq slime-lisp-implementations
        `((sbcl ("sbcl"))
          (ecl ("ecl"))
          (abcl ("abcl")))
        slime-default-lisp 'sbcl))

(with-eval-after-load 'slime-repl
  ;; NOTE: type [,] runs the command slime-handle-repl-shortcut
  (define-key slime-repl-mode-map (kbd "C-c C-q") 'slime-repl-sayoonara))

(with-eval-after-load "hyperspec"
  (let ((clhs-root (expand-file-name "~/Dropbox/Downloads/HyperSpec-7-0/HyperSpec/")))
    (setq common-lisp-hyperspec-root (concat "file://" clhs-root)
          common-lisp-hyperspec-symbol-table (expand-file-name "Data/Map_Sym.txt" clhs-root))))

;; SLY is Sylvester the Cat's Common Lisp IDE
;; https://github.com/capitaomorte/sly
(use-package sly
  :disabled
  :pin #:melpa-stable
  :init (remove-hook 'lisp-mode-hook 'slime-lisp-mode-hook)
  :config
  (setq sly-lisp-implementations
        `((abcl ("abcl"))
          (ecl ("ecl"))
          (sbcl ("sbcl")))
        sly-default-lisp 'sbcl)
  (sly-setup '(sly-fancy)))
