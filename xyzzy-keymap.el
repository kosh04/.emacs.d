;;; -*- coding:utf-8 -*-
;;; xyzzy-keymap.el - キーバインド関連

;; This file is NOT part of Emacs.

;; Time-stamp: <2009-07-12T23:21:02>

;;; Code:
(provide 'xyzzy-keymap)

;; (eval-when-compile (require 'cl))

;; lisp-mode-map
;; lisp-mode-shared-map
;; ielm-map
(defun elisp:define-key (key def)
  (mapc (lambda (keymap)
          (define-key keymap key def))
        (list lisp-interaction-mode-map
	      emacs-lisp-mode-map))
  def) 

;; @@標準関数から
(dolist (key '(?\C-1 ?\C-2 ?\C-3 ?\C-4 ?\C-5 ?\C-6 ?\C-7 ?\C-8 ?\C-9 ?\C-0))
  (and (eq (lookup-key global-map (vector key)) 'digit-argument)
       (global-unset-key (vector key))))

(global-set-key (kbd "C-h") 'delete-backward-char)
(define-key isearch-mode-map (kbd "C-h") 'isearch-delete-char)

(global-set-key (kbd "M-p") 'repeat-complex-command)
(global-set-key (kbd "C-z") 'scroll-down)
(global-set-key (kbd "C-x a") 'set-variable)
(global-set-key (kbd "C-x C-z") 'shrink-window)
(global-set-key (kbd "C-M-h") 'backward-kill-word)

;; (global-set-key [f10] 'first-error)
;; (global-set-key [f11] 'next-error)

;; @@Frame
;; (define-key ctl-x-map "6" 'ctl-x-5-prefix)
;; (define-key ctl-x-map "5" 'split-window-horizontally)
;; (define-key ctl-x-map "3" 'undefined)

;; @@SLIME
;; slime-compile-and-load-file
(elisp:define-key (kbd "C-c C-k") 'emacs-lisp-byte-compile-and-load)
(elisp:define-key (kbd "C-c C-i") 'lisp-complete-symbol) ; C-c TAB

(global-set-key (kbd "<M-f4>") 'kill-emacs)
(global-set-key (kbd "<ESC> <f4>") 'kill-emacs)

;; @@自分で定義したもの
(when (featurep 'xyzzy)
  (global-set-key (kbd "C-x p") 'move-previous-window)
  (global-set-key (kbd "<S-C-down>") 'scroll-up-both-window)
  (global-set-key (kbd "<S-C-up>") 'scroll-down-both-window)
  (global-set-key (kbd "<mouse-3>") 'bingalls-edit-menu)
  (global-set-key (kbd "M-]") 'goto-matched-parenthesis)

  (global-set-key (kbd "<RET>") 'newline-and-indent)

  (elisp:define-key (kbd "C-c C-m") 'elisp-macroexpand-1)
  (elisp:define-key (kbd "C-c M-m") 'elisp-macroexpand-all)
  ;; (elisp:define-key (kbd "C-c h") 'info-lookup-symbol) ; help-S
  )

;;; xyzzy-keymap.el ends here.
