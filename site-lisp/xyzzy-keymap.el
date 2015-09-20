;;; xyzzy-keymap.el - キーバインド関連

;; This file is NOT part of Emacs.

;;; Code:

;; lisp-mode-map
;; lisp-mode-shared-map
;; ielm-map
(defun elisp:define-key (key def)
  (mapc (lambda (keymap)
          (define-key keymap key def))
        (list lisp-interaction-mode-map
	      emacs-lisp-mode-map))
  def) 

;; Built-in

(dolist (key '("C-1" "C-2" "C-3" "C-4" "C-5" "C-6" "C-7" "C-8" "C-9" "C-0"))
  (and (eq (lookup-key global-map (kbd key)) 'digit-argument)
       (global-set-key (kbd key) nil)))

(global-set-key (kbd "C-h") 'delete-backward-char)
(define-key isearch-mode-map (kbd "C-h") 'isearch-delete-char)

(global-set-key (kbd "M-p") 'repeat-complex-command)
(global-set-key (kbd "C-z") 'scroll-down)
(global-set-key (kbd "C-x a") 'set-variable)
(global-set-key (kbd "C-x C-z") 'shrink-window)
(global-set-key (kbd "C-M-h") 'backward-kill-word)

(global-set-key (kbd "<f10>") 'first-error)
(global-set-key (kbd "<f11>") 'next-error)

;; Frame

;; (define-key ctl-x-map "6" 'ctl-x-5-prefix)
;; (define-key ctl-x-map "5" 'split-window-horizontally)
;; (define-key ctl-x-map "3" 'undefined)

;; SLIME

;; slime-compile-and-load-file
(elisp:define-key (kbd "C-c C-k") 'emacs-lisp-byte-compile-and-load)
(elisp:define-key (kbd "C-c C-b") 'eval-buffer)
(elisp:define-key (kbd "C-c C-i") 'lisp-complete-symbol) ; C-c TAB

(global-set-key (kbd "<M-f4>") 'save-buffers-kill-terminal)
(global-set-key (kbd "<ESC> <f4>") 'save-buffers-kill-terminal)

(with-eval-after-load 'xyzzy
  (global-set-key (kbd "C-x p") 'move-previous-window)
  ;; (global-set-key "\C-xp" "\C-u-1\C-xo") == move-previous-window
  (global-set-key (kbd "<S-C-down>") 'scroll-up-both-window)
  (global-set-key (kbd "<S-C-up>") 'scroll-down-both-window)
  (global-set-key (kbd "<down-mouse-3>") 'bingalls-edit-menu)
  (global-set-key (kbd "M-]") 'goto-matched-parenthesis)
  (global-set-key (kbd "C-x |") 'filter-region)
  (global-set-key (kbd "C-x c") 'run-console)

  (global-set-key (kbd "<RET>") 'newline-and-indent)

  ;; (elisp:define-key (kbd "C-c C-m") 'elisp-macroexpand-1)
  ;; (elisp:define-key (kbd "C-c M-m") 'elisp-macroexpand-all)
  ;; (elisp:define-key (kbd "C-c h") 'info-lookup-symbol) ; help-S
  )

(if (fboundp 'elisp:define-key)
    (fmakunbound 'elisp:define-key))

(provide 'xyzzy-keymap)

;;; xyzzy-keymap.el ends here.
