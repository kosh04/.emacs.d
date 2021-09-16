;;; site-lisp/user-utils.el

;;; 雑多な関数群 or 自作関数に依存するもの

(require 'subr-x)
(require 'cl-lib)

(declare-function json-read "json")

(defalias 'standard-error-output #'external-debugging-output
  "Standard error output (stderr).
Example:
  (print \"HELLO\" #\\='standard-error-output)
  (let ((standard-output #\\='standard-error-output))
    (message \"HELLO\"))")

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
  (sha1 (with-temp-buffer
          (insert-file-contents-literally filename)
          (buffer-string))))

(defalias 'sha1sum #'sha1-file)
(defun sha1* (object)
  (sha1 object nil nil 'binary))

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
  "ポイント位置の数値を UNIX 時間とみなした場合の時刻を表示します."
  (interactive)
  (let ((n (thing-at-point 'number)))
    (message "%d => %s" n (iso8601 n))))

(defun thing-at-point-to (thing fn)
  "ポイント位置の情報を基に (funcall FN (thing-at-point THING)) の結果を表示する."
  (interactive
   (let ((things '(symbol list sexp defun filename url email word sentence whitespace line number page)))
     (list (intern (completing-read "Thing at point as: " things nil t))
           (intern (completing-read "To command: " obarray 'fboundp t)))))
  (let* ((val (thing-at-point thing))
         (ans (funcall fn val)))
    (message "(funcall #'%S %S) ;=> %S" fn val ans)))

(defun point-of (fn)
  "FN 実行後のポイント位置を返す.

(point-of #\\='beginning-of-buffer) => 1"
  (save-window-excursion
    (save-excursion
      (funcall fn)
      (point))))

(defun delete-backward-word (&optional n)
  "直前の単語を削除する."
  (interactive "p")
  (unless (integerp n)
    (signal 'wrong-type-argument `(integerp ,n)))
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
property list is (:key value ...)
association list is ((key . value) ...)"
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
  (let ((emacs (expand-file-name invocation-name invocation-directory)))
    (with-help-window (help-buffer)
      (call-process emacs nil standard-output t "--help"))))

(defun emacsclient-help-options ()
  "[user] Display \"emacsclient --help\" text."
  (interactive)
  (let ((emacs (expand-file-name "emacsclient" invocation-directory)))
    (with-help-window (help-buffer)
      (call-process emacs nil standard-output t "--help"))))

(defun user::exec* (program infile &rest args)
  (with-output-to-string
    (apply #'call-process program infile standard-output t args)))

(defun user::exec (program input &rest args)
  "Execute PROGRAM+ARGS, then return the result as string.
INPUT (filename/buffer/nil) is used as a process standard input."
  (if (bufferp input)
      (let ((infile (make-temp-file "xyz")))
        (unwind-protect
            (progn
              (with-temp-file infile
                (insert-buffer-substring input))
              (apply #'user::exec* program infile args))
          (and (file-exists-p infile)
               (delete-file infile))))
    (apply #'user::exec* program input args)))

;; TODO: suppress other echo-area output (eldoc, etc)
(define-minor-mode what-cursor-position-minor-mode
  "Turn on/off `what-cursor-position'."
  nil " 🔍" nil
  (if what-cursor-position-minor-mode
      (add-hook 'post-command-hook #'what-cursor-position t 'local)
    (remove-hook 'post-command-hook #'what-cursor-position 'local)))

(defun xlfd-at (pos)
  (if (= pos (point-max))
      "[EOF]"
    (if-let ((font (font-at pos)))
        (font-xlfd-name font)
      "[Tofu]")))

(defun user::process-environment-alist ()
  (mapcar (lambda (e)
            (if-let ((pos (seq-position e ?=)))
                (cons (substring e 0 pos) (substring e (1+ pos)))
              (cons e nil)))
          process-environment))

(defun user::jump-to-env-directory (env-name)
  "環境変数 `ENV-NAME' が示すディレクトリにジャンプします."
  (interactive
   (list (completing-read "Jump to Env Directory: "
           (cl-remove-if-not
            (pcase-lambda (`(,_name . ,val))
              (and (stringp val)
                   (file-directory-p val)))
            (user::process-environment-alist)))))
  (dired (getenv env-name)))

(defun iterate-text-prop (prop func)
  "Iterate each buffer property PROP, and run with function FUNC.
URL `http://emacs.g.hatena.ne.jp/kiwanami/20110809/1312877192'"
  (cl-loop with pos = (point-min)
           for next = (next-single-property-change pos prop)
           for text-val = (and next (get-text-property next prop))
           while next do
           (when text-val
             (funcall func pos next text-val))
           (setq pos next)))

;; e.g. face の付いている領域についてループする
'
(iterate-text-prop 'face 
  (lambda (begin end val)
    (let ((print-escape-newlines t))
      (and val (message ">> %S : %S" val (buffer-substring-no-properties begin end))))))

(defun shredder (string)
  "文字列 STRING を伏せ字に変換します."
  (replace-regexp-in-string "[[:alnum:]]" "*" string))

(defun shredder-region (start end)
  "選択した範囲を******します."
  (interactive "*r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (while (re-search-forward "[[:alnum:]]" nil t)
        (replace-match "*")))))

(defun outlineapi-fetch (article-url)
  (let ((url-request-extra-headers
         '(("Referer" . "https://outline.com/")))
        (url (concat "https://api.outline.com/v3/parse_article?"
                     (url-build-query-string
                      `((source_url ,article-url))))))
    (with-temp-buffer
      (url-insert-file-contents url)
      (json-read))))

(defun user::url-parse (url)
  "クエリ文字列を含めたURLの解析を行います.
Return-Type => ((urlobj struct-url) . (query alist))"
  (pcase-let* ((urlobj (url-generic-parse-url url))
               (`(,path . ,query-string) (url-path-and-query urlobj))
               (query (if query-string (url-parse-query-string query-string))))
    (setf (url-filename urlobj) path)
    (cons urlobj query)))

(defun user::unique-color-name (obj)
  "Return a unique-like supported color name from OBJ."
  (let* ((colors (defined-colors))
         (n (mod (sxhash obj) (length colors))))
    (nth n colors)))

(defun user::unique-color-hex (obj)
  "Return a unique-like hexadecimal color identifier from OBJ."
  (let* ((ncolors (display-color-cells))
         (rgb (mod (sxhash obj) (1- ncolors)))
         (fmt (cond
                ((= ncolors 16777216) "#%06x") ; 24-bit #RRGGBB
                ((= ncolors      256) "#%03x") ; 8-bit  #RGB
                )))
    (format fmt rgb)))

(defun gc-with-message ()
  "Show memory usage.
See URL `https://www.reddit.com/r/emacs/comments/ck4zb3/'
Or `chart-emacs-storage'"
  (interactive)
  (display-message-or-buffer
   (cl-loop for (name size used free) in (garbage-collect)
            for used = (* used size)
            for free = (* (or free 0) size)
            for total = (file-size-human-readable (+ used free))
            for used = (file-size-human-readable used)
            for free = (file-size-human-readable free)
            concat (format "%s: %s + %s = %s\n" name used free total))))

;; `defface' の "face spec" ってどうやって調べるの？
;; (user::face-spec-display)
;; => ((type mac) (class color) (background dark) (min-colors 16777216))
(defun user::face-spec-display (&optional frame)
  "Return \"face spec\" for current match."
  `((type ,(or (window-system frame) 'tty))
    (class ,(frame-parameter frame 'display-type))
    (background ,(frame-parameter frame 'background-mode))
    (min-colors ,(display-color-cells frame))
    ;;(supports ,(display-supports-face-attributes-p ATTRIBUTES))
    ))

;; e.g. (radix 1234567890 36) => "#36rKF12OI"
(declare-function math-format-radix "calc-bin")
(defun radix (n &optional r)
  "数値 N を 基数 R に基づいてフォーマットします.
R のデフォルトは 10 (十進数; #10rN) です."
  (require 'calc-bin)
  (unless r (setq r 10))
  (let ((calc-number-radix r))
    (format "#%dr%s" r (math-format-radix n))))

;; 以下のようにコマンドラインから呼び出すと任意の Unicode 文字をクリップボードにコピーできる
;; emacsclient --eval "(copy-char)"
(defun copy-char ()
  "Copy characters.
URL `https://www.reddit.com/r/emacs/comments/f2ut1v/searching_and_typing_unicode_characters_like/'"
  (interactive)
  (x-focus-frame (selected-frame))
  (with-temp-buffer
    (call-interactively 'insert-char)
    (kill-ring-save (point-min) (point-max))
    (message "Copy %s" (car kill-ring))))

;; frame-utils.el (あるいは eyebrowse)
(defun rename-frame (name &optional frame)
  (interactive "sRename frame: ")
  (unless frame
    (setq frame (selected-frame)))
  (modify-frame-parameters frame `((name . ,name))))
(global-set-key (kbd "^X 5 ,") #'rename-frame)

(defun switch-to-frame (name)
  (interactive (list (completing-read "Frame: "
                       (mapcar (lambda (f)
                                 (frame-parameter f 'name))
                               (frame-list)))))
  (select-frame-set-input-focus
   (seq-find (lambda (f)
               (string= name (frame-parameter f 'name)))
             (frame-list))))
(global-set-key (kbd "^X 5 b") 'switch-to-frame)

;; (letter-spacing "HELLO WORLD") ;=> "H E L L O W O R L D"
(defun letter-spacing (str &optional pad)
  "文字 STR の隙間に PAD を埋め込み間隔を開ける."
  (unless pad
    (setq pad " "))
  (with-output-to-string
    (let (prev)
      (mapc (lambda (c)
              (if (and prev
                       (not (memq prev #1='(?\s ?\C-i ?\C-j ?\C-m)))
                       (not (memq c #1#)))
                  (princ pad))
              (write-char (upcase c))
              (setq prev c))
            str))
    (buffer-string)))

(defun transpose-window ()
  (interactive)
  (error "TODO: Not Implemented"))

(defun user::sudo-edit (filename)
  (interactive "fFind file @sudo: ")
  (find-file "/sudo:root@localhost:" filename))

(defun the-the ()
  "Search forward for for a duplicated word.
URL: `https://www.gnu.org/software/emacs/manual/html_node/eintr/the_002dthe.html'"
  (interactive)
  (message "Searching for for duplicated words ...")
  (push-mark)
  ;; This regexp is not perfect
  ;; but is fairly good over all:
  (if (re-search-forward
       "\\b\\([^@ \n\t]+\\)[ \n\t]+\\1\\b" nil 'move)
      (message "Found duplicated word.")
    (message "End of buffer")))

;; M-s o (occur) でも十分？
(define-advice isearch-done (:after (&rest _args) sync-view-search-string)
  "isearch と view-mode の検索文字列を中途半端に同期する."
  (when-let ((str isearch-string))
    (setq view-last-regexp (regexp-quote str))))

(defmacro csetq (symbol value)
  "Set SYMBOL to VALUE."
  `(customize-set-variable ',symbol ,value))

(provide 'user-utils)

;;; user-utils.el ends here
