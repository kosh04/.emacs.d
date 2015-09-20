;; config/misc.el

(add-hook 'emacs-startup-hook
          #'(lambda ()
              (message "Emacs init time: %s" (emacs-init-time))))

;; anzu (isearch utility)
(use-package anzu
  :diminish anzu-mode
  :config (progn
            (global-anzu-mode +1)
            (setq anzu-search-threshold 1000)
            (setq anzu-use-migemo (featurep 'migemo))
            (setq anzu-replace-to-string-separator " => "))
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp))
  :ensure t)

;; JSON
(require 'json)
(defalias 'json-decode 'json-read-from-string)

(defadvice locate (around modify-buffer-name activate)
  (let ((locate-buffer-name (format "*Locate %s*" search-string)))
    ad-do-it))

(defun iso8601 (&optional time universal)
  "Return ISO 8601 format time."
  ;; (format-time-string "%Y-%m-%dT%H:%M:%S")
  (format-time-string "%FT%T%z" time universal))

;; http://www.bookshelf.jp/texi/elisp-manual-20-2.5-jp/elisp_38.html#SEC610
(defun user:insert-time ()
  "日付と時刻の挿入."
  (interactive)
  (insert (iso8601)))

(defalias 'insert-time #'user:insert-time)
(global-set-key (kbd "C-c t") 'insert-time)

(defun my:delete-backward-word (&optional n)
  "直前の単語を削除する."
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
