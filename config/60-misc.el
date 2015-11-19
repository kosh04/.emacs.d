;; config/misc.el

(add-hook 'emacs-startup-hook
          #'(lambda ()
              (message "Emacs init time: %s" (emacs-init-time))))

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

(defun delete-backward-word (&optional n)
  "[user] 直前の単語を削除する."
  (interactive "p")
  (unless (integerp n)
    (signal 'wrong-type-argument (list 'integerp n)))
  (delete-region (point)
                 (progn (backward-word n) (point))))

;; C-w で直前の単語を削除する (bash)
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

;; elisp インデントを調整
(setf (get 'font-lock-add-keywords 'lisp-indent-function) 1)
(setf (get 'completing-read 'lisp-indent-function) 1)

;; Elnode パッチ
(with-eval-after-load 'elnode

(defun elnode--http-send-bytes (f httpcon text)
  "[user] monkey patch for `elnode-http-send-string' as raw bytes.
see also https://github.com/nicferrier/elnode/pull/101"
  (funcall f httpcon (encode-coding-string text 'raw-text)))

(advice-add 'elnode-http-send-string :around 'elnode--http-send-bytes)
;;(advice-remove 'elnode-http-send-string 'elnode--http-send-as-bytes)
)
