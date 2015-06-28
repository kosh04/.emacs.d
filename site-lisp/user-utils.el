;;; site-lisp/user-utils.el

(defun sequence (from to &optional step)
  (if (< from to)
      (number-sequence from to step)
      (nreverse (number-sequence to from step))))

(defun indent-line-sexp ()
  "長い一行S式をそれなりにインデントします."
  (interactive)
  (save-excursion
    (save-restriction
      ;; (while (condition-case nil (backward-up-list) (error nil)))
      (narrow-to-region (point)
                        (save-excursion
                          (forward-sexp)
                          (point)))
      (perform-replace ") " ")\n" nil  nil nil)))
  (indent-sexp))

(fset 'trim-tailing-whitespace #'delete-trailing-whitespace)

(defun insert-zenkaku-space ()
  "全角空白を挿入."
  (interactive)
  (insert (japanese-zenkaku " ")))

(fset 'insert-full-width-space #'insert-zenkaku-space)
(fset 'hankaku-region #'japanese-hankaku-region)
(fset 'zenkaku-region #'japanese-zenkaku-region)

(defun sha1-file (filename)
  "ファイルのチェックサムを出力します."
  (require 'sha1)
  (sha1 (let ((coding-system-for-write 'binary))
          (with-temp-buffer
            (insert-file-contents filename)
            (buffer-string)))))

(defalias 'sha1sum #'sha1-file)

(defun describe-bindings-anymap (keymap)
  (interactive (list (intern (completing-read "Keymap: " (apropos-internal "-map\\'")))))
  (cl-assert (keymapp (symbol-value keymap)))
  (with-help-window (help-buffer)
    (with-current-buffer (help-buffer)
      (insert (substitute-command-keys (format "\\{%s}" keymap))))))

(provide 'user-utils)

;;; user-utils.el ends here
