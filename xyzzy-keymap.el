;;; xyzzy-keymap.el - キーバインド関連

;; This file is NOT part of Emacs.

;; Time-stamp: <2009-04-24T10:04:39JST>

;;; Code:
(provide 'xyzzy-keymap)

;; (eval-when-compile (require 'cl))

;; 標準関数から
(dolist (key '(?\C-1 ?\C-2 ?\C-3 ?\C-4 ?\C-5 ?\C-6 ?\C-7 ?\C-8 ?\C-9 ?\C-0))
  (and (eq (lookup-key global-map (vector key)) 'digit-argument)
       (global-unset-key (vector key))))

(global-set-key (kbd "C-h") 'delete-backward-char)
(define-key isearch-mode-map (kbd "C-h") 'isearch-delete-char)

(global-set-key (kbd "M-p") 'repeat-complex-command)
(global-set-key (kbd "C-z") 'scroll-down)
(global-set-key (kbd "C-x a") 'set-variable)
(global-set-key (kbd "C-x C-c") 'iconify-or-deiconify-frame)
(global-set-key (kbd "C-x C-z") 'shrink-window)
(global-set-key (kbd "C-M-h") 'backward-kill-word)

(global-set-key (kbd "M-<f4>") 'kill-emacs)

;; 自分で定義したもの
(when (featurep 'xyzzy)
  (global-set-key (kbd "C-x p") 'move-previous-window)
  (global-set-key (kbd "S-C-<down>") 'scroll-up-both-window)
  (global-set-key (kbd "S-C-<up>") 'scroll-down-both-window)
  
  (dolist (keymap (list lisp-interaction-mode-map
                        emacs-lisp-mode-map
                        ;; lisp-mode-map
                        ;; lisp-mode-shared-map
                        ;; ielm-map
                        ))
    (define-key keymap (kbd "C-m") 'newline-and-indent)
    (define-key keymap (kbd "C-c C-m") 'elisp-macroexpand-1)
    (define-key keymap (kbd "C-c M-m") 'elisp-macroexpand-all)
    (define-key keymap (kbd "C-c h") 'info-lookup-symbol))
  
  (global-set-key (kbd "<mouse-3>") 'bingalls-edit-menu)
  (global-set-key (kbd "M-]") 'goto-matched-parenthesis)
  )

;;; xyzzy-keymap.el ends here.
