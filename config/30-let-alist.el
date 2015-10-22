;;; config/let-alist.el

(use-package let-alist
  :defer t
  :init
  ;; highlight .symbols
  (font-lock-add-keywords 'emacs-lisp-mode
    '(("\\_<\\.\\(?:\\sw\\|\\s_\\)+\\_>" 0
       font-lock-variable-name-face))))

