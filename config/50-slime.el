;;; config/slime.el

(use-package slime
  :config (progn
            (setq inferior-lisp-program "sbcl")
            (slime-setup '(slime-repl))))

(with-eval-after-load 'slime-repl
  ;; NOTE: type [,] runs the command slime-handle-repl-shortcut
  (define-key slime-repl-mode-map (kbd "C-c C-q") #'slime-repl-sayoonara))

;; SLY is Sylvester the Cat's Common Lisp IDE
;; https://github.com/capitaomorte/sly
(use-package sly
  :defer t)
