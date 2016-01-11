;;; config/slime.el

(use-package slime
  :disabled t
  :config
  (progn
    (slime-setup '(slime-repl slime-fancy))
    ;; https://common-lisp.net/project/slime/doc/html/Multiple-Lisps.html
    ;; C-u - M-x slime
    (setq slime-lisp-implementations
          `((sbcl ("sbcl"))
            (ecl ("ecl"))
            (abcl ("abcl")))
          slime-default-lisp 'sbcl)))

(with-eval-after-load 'slime-repl
  ;; NOTE: type [,] runs the command slime-handle-repl-shortcut
  (define-key slime-repl-mode-map (kbd "C-c C-q") 'slime-repl-sayoonara))

;; SLY is Sylvester the Cat's Common Lisp IDE
;; https://github.com/capitaomorte/sly
(use-package sly
  :defer t
  :config
  (progn
    (setq sly-lisp-implementations
          `((abcl ("abcl"))
            (ecl ("ecl"))
            (sbcl ("sbcl")))
          sly-default-lisp 'sbcl)
    (sly-setup '(sly-fancy)))
  :ensure nil)
