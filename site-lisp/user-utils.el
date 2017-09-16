;;; site-lisp/user-utils.el

;;; 雑多な関数群 or 自作関数に依存するもの

(require 'cl-lib)

(defun println (obj &optional out)
  (princ obj out)
  (terpri out))

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
  "あらゆる KEYMAP のキーマップを表示します."
  (interactive (list (intern (completing-read "Keymap: " obarray (lambda (s)
                                                                   (and (boundp s)
                                                                        (keymapp s)))))))
  (cl-check-type (symbol-value keymap) keymap)
  (with-help-window (help-buffer)
    (with-current-buffer (help-buffer)
      (insert (substitute-command-keys (format "\\{%s}" keymap))))))

(defun circular-list-p (object)
  "Returns true if OBJECT is a circular list, NIL otherwise."
  (and (listp object)
       (eq (cl-list-length object) nil)))

(defun arglist (def)
  "Get DEF arguments.

(arglist 'lambda)  => \"(lambda ARGS [DOCSTRING] [INTERACTIVE] BODY)\"
(arglist 'arglist) => (def)
"
  (when (fboundp def)
    (or (car (help-split-fundoc (documentation def) def))
        (help-function-arglist def))))

(defun iso8601 (&optional time universal)
  "Return ISO 8601 format time string."
  ;; (format-time-string "%Y-%m-%dT%H:%M:%S")
  (format-time-string "%FT%T%z" time universal))

(defun unix-time-at-point ()
  (interactive)
  (let ((n (thing-at-point 'number)))
    (message "%d => %s" n (iso8601 n))))

(defun point-of (fn)
  "FN 実行後のポイント位置を返す.

(point-of #'beginning-of-buffer) => 1"
  (save-window-excursion
    (save-excursion
      (funcall fn)
      (point))))

(defun delete-backward-word (&optional n)
  "直前の単語を削除する."
  (interactive "p")
  (unless (integerp n)
    (signal 'wrong-type-argument (list 'integerp n)))
  (delete-region (point)
                 (progn (backward-word n) (point))))


(defun plist->alist (plist)
  "Translate PLIST to alist.
property list (:key value ...)
association list ((key . value) ...)"
  (cl-labels ((symbol (sym)
                (if (keywordp sym) (intern (substring (symbol-name sym) 1)) sym)))
    (cl-loop for (key value) on plist by #'cddr
           collect (cons (symbol key) value))))

(defun alist->plist (alist)
  "Translate ALIST to plist.
property list (:key value ...)
association list ((key . value) ...)"
  (cl-labels ((keyword (sym)
                (if (keywordp sym) sym (intern (format ":%s" sym)))))
    (let (plist)
      (cl-loop for (key . value) in alist
               do (setq plist (plist-put plist (keyword key) value)))
      plist)))

;; http://ja.stackoverflow.com/a/24433/2391
(defun get-in (object keys)
  "Returns the value in a nested assoc or vector OBJECT used by KEYS.

Example:
(get-in object [key1 key2 0]) => value"
  (cl-reduce #'(lambda (obj key)
                 (cl-typecase key
                   (integer (elt obj key))
                   ((or symbol string) (cdr (assoc key obj)))
                   (otherwise (user-error "Unknown key type: %S" key))))
             keys
             :initial-value object))

(defun buffer-needs-save-p (buffer)
  "[user] Return non-nil if the visited-file BUFFER is still modified."
  (and (buffer-file-name buffer)
       (buffer-modified-p buffer)))

;;(seq-filter #'buffer-needs-save-p (buffer-list))

(defun occur-all-buffers (regexp)
  "[user] Search REGEXP in all buffers."
  (interactive "sSearch buffers: ")
  (multi-occur (buffer-list) regexp)
  ;;(multi-occur-in-matching-buffers "." regexp t)
  )

(defun module-feature-p ()
  (fboundp 'module-load))

(defun Math.random ()
  "Return [0.0, 1.0) real number."
  (/ (random) (+ 1.0 most-positive-fixnum)))

(defun rand (min max)
  "Return [MIN, MAX] random number."
  (+ min (% (random) (+ max 1 (- min))))
  ;;(+ min (floor (* (random) (+ max (- min) 1.0)) (+ 1.0 most-positive-fixnum)))
  )

(defun rand* (min max)
  "Return [MIN, MAX) random number."
  (error "Not implement yet"))

(defun zalgo (text &optional depth)
  "Zalgo Text Generator (incomplete)."
  (replace-regexp-in-string
   ;;"[a-zA-Z0-9]"
   (rx alnum)
   (lambda (s)
     (concat s (with-output-to-string
                 (dotimes (_ (or depth 30))
                   (princ (string (rand #x0300 #x036F)))))))
   text t t))

(defun zalgo-buffer (&optional depth)
  (interactive "*")
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "[A-Za-z0-9]" nil t)
      (replace-match (concat (match-string 0)
                             (with-output-to-string
                               (dotimes (_ (or depth 5))
                                 (princ (string (rand #x0300 #x036F))))))
                     nil t))))

(defun emacs-help-options ()
  "[user] Display \"emacs --help\" text."
  (interactive)
  (let* ((emacs (expand-file-name (invocation-name) (invocation-directory)))
         (text (mapconcat 'identity (process-lines emacs "--help") "\n")))
    (with-help-window (help-buffer)
      (princ text))))

(provide 'user-utils)

;;; user-utils.el ends here
