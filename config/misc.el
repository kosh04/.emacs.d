;; config/misc.el

(defadvice locate (around modify-buffer-name activate)
  (let ((locate-buffer-name (format "*Locate %s*" search-string)))
    ad-do-it))

;; 日付と時刻の挿入
;; http://www.bookshelf.jp/texi/elisp-manual-20-2.5-jp/elisp_38.html#SEC610
(defun my:insert-time ()
  (interactive)
  (insert (let ((system-time-locale "C"))
            (format-time-string "%Y-%m-%dT%H:%M:%S"))))

(defalias 'insert-time #'my:insert-time)
(global-set-key (kbd "C-c t") 'insert-time)

(defun my:delete-backward-word (&optional n)
  (interactive "p")
  (unless (integerp n)
    (signal 'wrong-type-argument (list 'integerp n)))
  (delete-region (point)
                 (progn (backward-word n) (point))))

(defalias 'delete-backward-word 'my:delete-backward-word)
(define-key minibuffer-local-map (kbd "C-w") 'delete-backward-word)

;;; find-file-at-point
;; カーソル上にあるファイル名や URL を開く
;; フツーの Find file: は C-u C-x C-f
;; (require 'ffap)
;; (ffap-bindings)

(defun ffap-emacs ()
  "URLをemacsのバッファに開く."
  (interactive)
  (url-handler-mode t)
  (let ((ffap-url-fetcher #'find-file))
    (call-interactively #'ffap)))

;; font-lock
(put 'font-lock-add-keywords 'lisp-indent-function 1)

(use-package nyan-mode
  :if window-system
  :config (progn
            (setq nyan-bar-length 12)
            (nyan-mode +1))
  :ensure t)
