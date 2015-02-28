;;; config/slime.el

(use-package slime
  :init (progn
          (setq inferior-lisp-program "sbcl")
          (slime-setup '(slime-repl))))

(with-eval-after-load 'slime-repl
  ;; NOTE: type [,] runs the command slime-handle-repl-shortcut
  (define-key slime-repl-mode-map (kbd "C-c C-q") #'slime-repl-sayoonara))
