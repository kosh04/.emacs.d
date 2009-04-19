;;; xyzzy.el --- CommonLisp/SLIME/xyzzyの便利関数・キーマップをEmacsで使う

;; This file is NOT part of Emacs.

;; Time-stamp: <2009-04-19T19:50:58>

;; CLパッケージは関数をライブラリとして使用することは推奨されていない.
;; 代用するならloopマクロが考えられる

;;; Code:
(provide 'xyzzy)

(eval-when-compile
  (require 'cl)
  ;; (require 'ielm)
  )

;;; @@Data-Type
(fset 'compiled-function-p #'byte-code-function-p)

(defun alphanumericp (char)
  (if (string-match "[A-Za-z0-9]" (char-to-string char)) t nil))

(defun kanji-char-p (character)
  (check-type character character)
  (multibyte-string-p (string character)))

;;; @@Variable-and-Constant
(defvaralias '*modules* 'features)
(defvaralias '*load-path* 'load-path)
(defvaralias '*pre-startup-hook* 'before-init-hook) ; emacs-startup-hook ?
(defvaralias '*post-startup-hook* 'after-init-hook) ; window-setup-hook ?
(defvaralias 'si:*command-line-args* 'command-line-args-left) ; command-line-args ?
(defvaralias '*load-pathname* 'load-file-name)
(defvaralias '*etc-path* 'data-directory)

(defmacro defparameter (symbol value &optional doc-string)
  `(if (boundp ',symbol)
       (progn
         (set ',symbol ,value)
         ,(if doc-string
              `(put ',symbol 'variable-documentation ,doc-string))
         ',symbol)
       (defvar ,symbol ,value ,doc-string)))

;;; @@制御構造
;;; @@Package
;;; @@Function

;;; @@Macro
(defun macro-function (symbol &optional environment)
  (declare (ignore environment))
  (and (fboundp symbol)
       (let ((fn (symbol-function symbol)))
         (and (eq (car-safe fn) 'macro)
              fn))))

;;; @@Symbol

;;; @@Number
(defun logandc1 (x y) (logand (lognot y) y))
(defun logandc2 (x y) (logand x (lognot y)))
;; (defun logcount (integer) ())

;;; @@Character
;; (digit-char-p ?f 16) => 15
(defun* digit-char-p (char &optional (radix 10))
  (let ((pos (string-match (string (upcase char))
                           "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ")))
    (and pos (<= pos radix) pos)))

;; (digit-char 15 16) => 70 (?F)
(defun* digit-char (weight &optional (radix 10))
  (and (<= 0 weight) (< weight radix)
       (< 0 radix) (<= radix 36)        ; (length "012...WYX") => 36
       (elt "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" weight)))

(defun char-downcase (char) (check-type char character) (downcase char))
(defun char-upcase (char) (check-type char character) (upcase char))

(defun standard-char-p (char)
  "Return T if CHAR is [ -~] or Newline, otherwise NIL."
  (or (and (<= 32 char) (<= char 126))
      (= char 10)))

;;; @@Sequence
(defun remove-trail-slash (str)
  "STR の末尾に `/' があれば削除した文字列を返す。"
  (let ((len (length str)))
    (cond ((zerop len)
           "")
          ((char-equal (elt str (1- len)) ?/)
           (substring str 0 (1- len)))
          (t
           str))))

(defun append-trail-slash (str)
  (let ((len (length str)))
    (cond ((zerop len)
           "")
          ((not (char-equal (elt str (1- len)) ?/))
           (concat str "/"))
          (t
           str))))

(defun map-slash-to-backslash (string) (replace-regexp-in-string "/" "\\\\" string))
(defun map-backslash-to-slash (string) (replace-regexp-in-string "\\\\" "/" string))

(defun* string/= (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (eq (compare-strings string1 start1 end1 string2 start2 end2 nil) t))

(defun string-left-trim (char-bag string)
  (setq char-bag (let (acc)
                   (dotimes (n (length char-bag))
                     (push (elt char-bag n) acc))
                   (nreverse acc)))
  (let ((start 0)
        (end (length string)))
    (do ((index start (1+ index)))
        ((or (= index end)
             (null (memq (elt string index) char-bag)))
         (substring string index)))))

(defun string-right-trim (char-bag string)
  (setq char-bag (let (acc)
                   (dotimes (n (length char-bag))
                     (push (elt char-bag n) acc))
                   (nreverse acc)))
  (let ((start 0)
        (end (length string)))
    (do ((index (1- end) (1- index)))
        ((or (= index start)
             (null (memq (elt string index) char-bag)))
         (substring string start (1+ index))))))

(defun string-trim (char-bag string)
  (string-left-trim char-bag (string-right-trim char-bag string)))

;; substitute-string と replace-regexp-in-string が似ている気がする
;; (arglist 'substitute-string)        ; (string pattern replacement &key :case-fold :start :end :skip :count)
;; (arglist 'replace-regexp-in-string) ; (regexp rep string &optional fixedcase literal subexp start)
;; (replace-regexp-in-string "(foo).*'" "bar" " foo foo" nil nil 1) ; " bar foo"

;;; @@List
(fset 'safe-car #'car-safe)
(fset 'safe-cdr #'cdr-safe)

;;; @@Hash
;;; @@Array
;;; @@Chunc

;;; @@Eval
(defadvice eval-last-sexp (before eval-safe activate)
  "ポイントがシンボルの途中でもエラーにならない eval-last-sexp"
  ;; 他のモードでも使えるようにスキップする文字を明示的にすべきかも
  (skip-syntax-forward "w_"))
;; (ad-deactivate 'eval-last-sexp)

;;; @@Input/Output
(defun princ-to-string (object &optional stream)
  (with-output-to-string (princ object stream)))

;; (values)を使うpprintは実装できない？

;;; @@Filesystem
(fset 'merge-pathnames #'expand-file-name)
(fset 'file-namestring #'file-name-nondirectory)
(fset 'directory-namestring #'file-name-directory)
(fset 'get-buffer-file-name #'buffer-file-name)
(fset 'file-exist-p #'file-exists-p)
(fset 'pathname-type #'file-name-extension)

;; CLにファイルのリンク先を参照する関数はあるのか？
;; file-truename, file-chase-links
(defun* directory (pathname &key absolute recursive wild depth file-only
                            show-dots count directory-only callback file-info)
  (if file-info
      (directory-files-and-attributes pathname absolute wild)
      (directory-files pathname absolute wild)))

(defun pathname-name (pathname)
  (file-name-sans-extension (file-name-nondirectory pathname)))

(defun namestring (pathname)
  (expand-file-name pathname))

(defun probe-file (pathname)
  (and (file-exists-p pathname)
       (expand-file-name pathname)))

(defun truename (pathname)
  (or (probe-file pathname)
      (error "The file %s does not exist." pathname)))

(and (fboundp 'w32-short-file-name)
     (fset 'get-short-path-name #'w32-short-file-name))

;; ディレクトリが存在しているかは考えていないので
;; 末尾にパス区切りのない "~/lib/emacs" などは "emacs" をファイルとみなしている
;; (pathname-directory "~/lib/emacs")  => ("home" "lxuser" "lib")
;; (pathname-directory "~/lib/emacs/") => ("home" "lxuser" "lib" "emacs")
(defun pathname-directory (pathname)
  (let ((dir (split-string (file-name-directory (expand-file-name pathname))
                           "/" 'rmit-nulls)))
    (if (string-match "^[A-Za-z]:$" (car dir)) ; trim Device
        (cdr dir)
        dir)))

;; (sub-directory-p "~/lib/emacs/" "~/lib/emacs/") => t
;; (sub-directory-p "~/lib/emacs/" "~/lib/emacs")  => t
;; (sub-directory-p "~/lib/emacs" "~/lib/emacs/")  => t
(defun sub-directory-p (dir parent)
  "DIRECTORYがPARENTのサブディレクトリならt、そうでなければnilを返す。"
  (do ((x (pathname-directory (file-name-as-directory dir)) (cdr x))
       (y (pathname-directory (file-name-as-directory parent)) (cdr y)))
      ((null y) t)
    (if (or (null x)
            (not (string-equal (car x) (car y))))
        (return nil))))

;; シンボリックリンク考慮なし
(defun path-equal (pathname1 pathname2)
  "PATHNAME1とPATHNAME2が同じパスを指していればt、そうでなければnilを返します。"
  (setq pathname1 (expand-file-name pathname1)
        pathname2 (expand-file-name pathname2))
  (let* ((l1 (length pathname1))
         (l2 (length pathname2))
         (min (min l1 l2)))
    (and (eq (compare-strings pathname1 0 min
                              pathname2 0 min
                              'ignore-case)
             t)
         (or (= l1 l2)
             ;; 同じ名前のファイルとディレクトリは共存しないと仮定
             (and (= l1 (1+ l2))        ; "/hoge", "/hoge/"
                  (char-equal (elt pathname1 l2) ?/))
             (and (= l2 (1+ l1))        ; "/hoge/", "/hoge"
                  (char-equal (elt pathname2 l1) ?/))
             ))))

(defun compile-file-pathname (pathname)
  "Emacsでバイトコンパイルした時の出力ファイル名を返します."
  (if (string-match emacs-lisp-file-regexp pathname)
      (concat (expand-file-name pathname) "c")
      (concat (expand-file-name pathname) ".elc")))

(defun file-length (pathname)
  "Return PATHNAME size in bytes."
  (nth 7 (file-attributes pathname)))

;;; @@Error
(fset 'plain-error #'error)

;;; @@Window
(fset 'next-page #'scroll-up)
(fset 'previous-page #'scroll-down)

(defun move-previous-window (&optional arg)
  (interactive "p")
  (other-window (- arg)))
(global-set-key "\C-xp" 'move-previous-window)

(defun get-window-start-line ()
  (or (line-number-at-pos (window-start))
      (save-excursion
        (move-to-window-line 0)
        (line-number-at-pos))
      (if (pos-visible-in-window-p (point-min))
          1
          (save-excursion (while (pos-visible-in-window-p)
                            (line-move -1))
                          (1+ (line-number-at-pos))))))

(defun get-window-line (&optional window)
  "ウィンドウのカーソルの表示行を返します. [zero-origin]"
  ;; posn-col-row ?
  (cdr (posn-actual-col-row (posn-at-point (point) window))))

;; window-height はモードラインとヘッダとミニバッファと何を含む？
;; (- (window-height) (window-body-height))
;; ? (window-line-height)
(defun window-lines (&optional window)
  (or (destructuring-bind (left top right bottom)
          (window-edges)
        (- bottom top
           (if mode-line-format 1 0)
           (if header-line-format 1 0)))
      (window-body-height window)))

(defun pos-not-visible-in-window-p (&optional pos window partially)
  (not (pos-visible-in-window-p pos window partially)))

(defun scroll-window (arg)
  (check-type arg integer)
  (scroll-up arg))

;;  End of buffer, Beginning of buffer でウィンドウが移動してしまう
(defun scroll-up-both-window ()
  (interactive)
  (other-window 1)
  (scroll-window 2)
  (other-window -1)
  (scroll-window 2))

(defun scroll-down-both-window ()
  (interactive)
  (other-window 1)
  (scroll-window -2)
  (other-window -1)
  (scroll-window -2))

(global-set-key [S-C-down] 'scroll-up-both-window) ; #\S-C-Down
(global-set-key [S-C-up] 'scroll-down-both-window) ; #\S-C-Up

(put 'scroll-left 'disabled nil)        ; C-x <
;; (put 'scroll-right 'disabled nil)       ; C-x >

(fset 'toggle-ruler #'ruler-mode)
(fset 'toggle-cursor-line #'hl-line-mode) ; 下線カーソルface変更
(defun set-buffer-fold-type-none () (interactive) (toggle-truncate-lines 1))
(defun set-buffer-fold-type-window () (interactive) (toggle-truncate-lines -1))
(defun toggle-eof (&optional arg)
  (interactive "P")
  (setq default-indicate-empty-lines
        (or arg (not default-indicate-empty-lines))))

;;; @@Buffer
(fset 'selected-buffer #'current-buffer)
(fset 'find-buffer #'get-buffer)
(fset 'create-new-buffer #'generate-new-buffer)

(defun delete-buffer (buffer)
  "Kill the BUFFER without query."
  (let ((kill-buffer-query-functions nil))
    (kill-buffer buffer)))

(defun file-visited-p (&optional buffer)
  (if (buffer-file-name buffer) t nil))

;; (window-buffer-height (selected-window)) だとEOFのみの行を認識してくれない
(defun buffer-lines (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (line-number-at-pos (point-max))))

;; revert-buffer{,-with-coding-system}
;; おかしなエンコード指定をすると読み込み後のポイント位置がずれるんだが、なぜだ
(defun xyzzy-revert-buffer (&optional encoding)
  (interactive (list (if current-prefix-arg
                         (read-coding-system "Encoding: ")
                         ;; (intern (completing-read "Encoding: " '("sjis" "euc-jp" "utf-8") nil t))
                         buffer-file-coding-system)))
  (check-coding-system encoding)
  (let ((coding-system-for-read encoding))
    (revert-buffer nil t)))

(defun find-name-buffer (buffer-name)
  (let ((acc nil)
        (regexp (concat "^" (regexp-quote buffer-name))))
    (dolist (b (buffer-list))
      (if (string-match regexp (buffer-name b))
          (push b acc)))
    (nreverse acc)))

;; optporop.l backup.l
(defvaralias 'make-backup-file-always 'delete-old-versions
  "番号が最後までいったら勝手に詰める") ; ?

;; clear-undo-boundary

;; (list make-backup-files version-control kept-old-versions kept-new-versions 'pack-backup-file-name make-backup-file-always)

;;; @@Minibuffer

;;; @@Region
(defadvice kill-ring-save (after kill-ring-msg activate)
  ;; kill-new の方がいいのかな?
  (message "Region copied"))

(defvar *quotation-prefix* "| ")        ; or comment-start

(defun quote-region (from to)
  (interactive "*r")
  (save-restriction
    (narrow-to-region from to)
    (goto-char (point-min))
    (while (not (eobp))
      (insert *quotation-prefix*)
      (unless (forward-line 1)
	(return))))
  t)

;; buffer-read-onlyの時はちゃんとエラーにして、カーソルが動かないように
(defadvice kill-line (before kill-line-read-only activate)
  (barf-if-buffer-read-only))

;; 起動時から使えるように
(put 'upcase-region 'disabled nil)      ; C-x C-u
(put 'downcase-region 'disabled nil)    ; C-x C-l
(put 'narrow-to-region 'disabled nil)   ; C-x n n

;;; @@Mode
(fset 'delete-hook #'remove-hook)

;;; @@Syntax
(defmacro define-syntax-xxx-p (name c)
  `(defun ,(intern (format "syntax-%s-p" (symbol-name name)))
       (char &optional syntax-table)
     (with-syntax-table (or syntax-table (syntax-table))
       (eql (char-syntax char) ,c))))
;; ちゃんと並び替えよう
;; Cの/* 2文字コメントとかどうする？ */
;; http://www.fan.gr.jp/~ring/doc/elisp_20/elisp_35.html
(define-syntax-xxx-p whitespace ? )
(define-syntax-xxx-p word ?w)
(define-syntax-xxx-p symbol ?_)
(define-syntax-xxx-p punctuation ?.)
(define-syntax-xxx-p open ?\()
(define-syntax-xxx-p close ?\))
(define-syntax-xxx-p character-quote ?\") ; quote
(define-syntax-xxx-p escape ?\\)
(define-syntax-xxx-p start-comment ?\<)
(define-syntax-xxx-p end-comment ?\>)
(define-syntax-xxx-p expression-prefix ?') ; symbol-prefix
(define-syntax-xxx-p paired-delimiter ?$)  ; math
(define-syntax-xxx-p string-quote ?\")     ; string
(define-syntax-xxx-p junk ?x)
(define-syntax-xxx-p open-tag ?{)
(define-syntax-xxx-p close-tag ?})

;; lisp/expand.el を参考に
(defun parse-point-syntax (&optional point)
  (unless point (setq point (point)))
  (save-excursion
    (goto-char point)
    (let* ((lim (or (save-excursion
                      (beginning-of-defun)
                      (point))
                    (point-min)))
           (state (parse-partial-sexp lim point)))
      (cond
        ((nth 3 state) :string)
        ((nth 4 state) :comment)
        (t nil)))))

;;; @@Keymap
(defvaralias '*global-keymap* 'global-map)
(defvaralias 'spec-map 'mode-specific-map)
(defvaralias 'ctl-x-6-map '2C-mode-map) ; two-column.el

(fset 'lookup-key-command #'key-binding)
(fset 'lookup-keymap #'lookup-key)
(fset 'local-keymap #'current-local-map)
(fset 'minor-mode-map #'current-minor-mode-maps)

;; MINOR-MODE-KEYMAPS LOCAL-KEYMAP GLOBAL-KEYMAPの順に優先されます。らしい
;; 後で書き直し
;; (where-is-internal 'describe-function) => ([f1 102] [help 102] [menu-bar help-menu describe describe-function])
(defun command-keys (command global-keymap local-keymap &optional minor-mode-keymaps)
  (let (acc tmp)
    (setq tmp (where-is-internal command global-keymap))
    (dolist (x tmp) (pushnew x acc :test #'equalp))
    (setq tmp (where-is-internal command local-keymap))
    (dolist (x tmp) (pushnew x acc :test #'equalp))
    (when minor-mode-keymaps
      (setq tmp (where-is-internal command global-keymap))
      (dolist (x tmp) (pushnew x acc :test #'equalp)))
    (nreverse acc)))

(dolist (key '(?\C-1 ?\C-2 ?\C-3 ?\C-4 ?\C-5 ?\C-6 ?\C-7 ?\C-8 ?\C-9 ?\C-0))
  (and (eq (lookup-key global-map (vector key)) 'digit-argument)
       (global-unset-key (vector key))))
(dolist (keymap (list lisp-mode-map
                      lisp-interaction-mode-map
                      emacs-lisp-mode-map
                      ;; lisp-mode-shared-map
                      ;; ielm-map
                      ))
  (define-key keymap "\C-m" 'newline-and-indent))
(global-set-key "\M-p" 'repeat-complex-command)
(global-set-key "\C-z" 'scroll-down)
(global-set-key "\C-xa" 'set-variable)
(global-set-key "\C-x\C-c" 'iconify-or-deiconify-frame)
(global-set-key "\C-x\C-z" 'shrink-window)
(define-key esc-map "\C-h" 'backward-kill-word) ; [?\C-\M-h]

(global-set-key [M-f4] 'kill-emacs)

;;; @@Text
;; あってないかも
(defun convert-encoding-to-internal (encoding input-string &optional output-stream)
  (declare (ignore output-stream))
  (encode-coding-string input-string encoding))

(defun convert-encoding-from-internal (encoding input-string &optional output-stream)
  (declare (ignore output-stream))
  (decode-coding-string input-string encoding))

(defun map-internal-to-utf-8 (input-string &optional output-stream)
  (convert-encoding-to-internal 'utf-8 input-string))

(defun map-char-encoding-region (p1 p2 &optional encoding)
  (interactive (list (region-beginning)
                     (region-end)
                     (if current-prefix-arg
                         (read-non-nil-coding-system "Encoding: "))))
  (barf-if-buffer-read-only)
  (check-coding-system encoding)
  (decode-coding-region p1 p2 encoding))

(fset 'detect-char-encoding #'detect-coding-string) ; ?

;;; @@Search, Regexp
(fset 'ed::protect-match-data 'save-match-data)

(defun looking-for (string &optional case-fold)
  (save-match-data                      ; 必要?
    (let ((case-fold-search case-fold))
      (looking-at (regexp-quote string)))))

(defun xyzzy-looking-back (string &optional case-fold)
  (save-excursion
    (condition-case nil
        (progn
          (backward-char (length string))
          (looking-for string case-fold))
      (error nil))))

;;; @@Dialog

;;; @@Date-Time
;;; 時刻は UNIX time なので注意
(fset 'get-universal-time #'current-time)
(fset 'encode-universal-time #'encode-time)
(fset 'decode-universal-time #'decode-time)
(fset 'format-date-string #'format-time-string)
(fset 'get-internal-real-time #'get-internal-run-time) ; ?
(defun get-decoded-time () (decode-time (current-time)))

;; ? タイマーの書式 [...] を調べよう
(defun start-timer (interval fn &optional one-shot-p)
  (run-at-time t (if one-shot-p nil interval) fn))
(fset 'stop-timer #'cancel-function-timers)
(defun stop-all-timers () (mapc #'cancel-timer timer-list))

;;; @@Menu

;;; @@Filer
;;; filer [xyzzy] <-> dired [emacs]
(eval-when-compile
  (require 'dired))

(defvaralias 'filer-keymap 'dired-mode-map)

(fset 'filer-mark-path-mask #'dired-mark-files-regexp) ; % m

(defun filer-get-current-file ()
  (dired-get-filename nil 'no-error))

;;; @@Position
(defun goto-matched-parenthesis ()
  "Go to the matching parenthesis if on parenthesis."
  (interactive)
  (cond ((looking-at "[([{]")
         (forward-sexp) (backward-char))
        ((looking-at "[])}]")
         (forward-char) (backward-sexp))
        (t nil)))
(global-set-key [?\M-\]] 'goto-matched-parenthesis)

(defun current-line-number ()
  (line-number-at-pos (point)))

(defun goto-bol () (goto-char (line-beginning-position)))
(defun goto-eol () (goto-char (line-end-position)))

;;; @@Process
;; 試してません
(defun execute-subprocess (cmd &optional arg bufname environ directory)
  (let ((process-environment (append (mapcar #'(lambda (env)
                                                 (format "%s=%s" (car env) (cdr env)))
                                             environ)
                                     process-environment))
        (default-directory (or directory default-directory)))
    (setq bufname (or bufname"*Command Output*"))
    (start-process-shell-command "Shell" bufname cmd arg)
    (get-buffer bufname)))

;;; @@System
(defun find-load-path (filename)
  (locate-library filename))

;;; @@Misc
(fset 'modulep #'featurep)
(fset 'gc #'garbage-collect)
(defun etc-path () data-directory)

(defun update-mode-line (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (force-mode-line-update)))

(defun refresh-screen (&optional f)
  (redraw-display)
  (redraw-frame (selected-frame)))

(defun autoload-function-p (def)
  (eq (car-safe (symbol-function def)) 'autoload))

;;; @@Xyzzy-only
(defvar *kill-buffer-kills-scratch* nil
  "non-nilならば、kill-bufferで*scratch*が削除可能.")

(defun kill-scratch-hook ()
  (cond (*kill-buffer-kills-scratch*
         t)
        ((and (eq major-mode 'lisp-interaction-mode)
              (equal (buffer-name (current-buffer)) "*scratch*"))
         (erase-buffer)
         (when (and initial-scratch-message
                    (not inhibit-startup-message))
           (insert initial-scratch-message))
         nil)
        (t t)))
(add-hook 'kill-buffer-query-functions 'kill-scratch-hook)

(defun* string-downcase (string &key (start 0) end)
  (concat (substring string 0 start)
          (downcase (substring string start end))
          (if end (substring string end) "")))

;; 正規表現のコンパイルってあったっけ？
(defun compile-regexp (regexp &optional case-fold) regexp)

;; 未完 ポイント位置でなくマウスの位置に表示される
;; left,topはピクセル指定？
(defun popup-string (string point &optional timeout)
  (save-excursion
    (cond ((pos-visible-in-window-p point)
           (goto-char point))
          ((< (point) point)
           (move-to-window-line -1))
          (t
           (move-to-window-line 0)))
    (destructuring-bind (x . y)
        (posn-x-y (posn-at-point))
      (let ((tooltip-hide-delay (or timeout tooltip-hide-delay))
            (tooltip-frame-parameters (append `((left . ,x))
                                              `((top . ,y))
                                              tooltip-frame-parameters)))
        (tooltip-show string)))))

(autoload 'trace-is-traced "trace")
(autoload 'untrace-all "trace")
(autoload 'untrace-function "trace")

(defmacro trace (&rest function-name)
  (if function-name
      `(mapc #'trace-function ',function-name)
      `(let (fns)
         (ad-do-advised-functions (fn)
           (if (trace-is-traced fn) (push fn fns)))
         (nreverse fns))))

(defmacro untrace (&rest function-name)
  (if function-name
      `(mapc #'untrace-function ',function-name)
      `(untrace-all)))

(defvar *elisp-macroexpand-require-cl-function* t)
;; (setq *elisp-macroexpand-require-cl-function* nil)

;;; ? Warning: the function `cl-prettyexpand' might not be defined at runtime.
(autoload 'cl-prettyexpand "cl-extra")
(autoload 'cl-prettyprint "cl-extra")
(autoload 'cl-macroexpand-all "cl-extra")

;; slime-macroexpand-1
(defun elisp-macroexpand-1 (&optional repeatedly)
  (interactive "P")
  (let ((form (car (save-excursion
                     (read-from-string
                      (buffer-substring (progn
                                          ;; looking-at
                                          (unless (syntax-open-p (following-char))
                                            (up-list -1))
                                          (point))
                                        (progn
                                          (forward-list)
                                          (point)))
                      ;; (thing-at-point 'list)
                      ))))
        (print-circle t)
        (temp-buffer-setup-hook '(emacs-lisp-mode))
        ;; 色付けがなぜ効かない
        (temp-buffer-show-hook  '(font-lock-fontify-buffer)))
    (unless form
      (return-from elisp-macroexpand-1))
    (with-output-to-temp-buffer " *ELISP macroexpantion*"
      (princ (with-temp-buffer
               (if *elisp-macroexpand-require-cl-function*
                   (cl-prettyexpand form repeatedly)
                   (cl-prettyprint (funcall (if repeatedly
                                                #'cl-macroexpand-all
                                                #'macroexpand)
                                            form)))
               (buffer-string))))))

(defun elisp-macroexpand-all ()
  (interactive)
  (elisp-macroexpand-1 t))

(dolist (keymap (list emacs-lisp-mode-map
                      lisp-interaction-mode-map                      
                      ;; ielm-map
                      ))
  (define-key keymap "\C-c\C-m" 'elisp-macroexpand-1)
  (define-key keymap "\C-c\M-m" 'elisp-macroexpand-all)
  (define-key keymap "\C-ch" 'info-lookup-symbol))

;; だめ
;; (fset 'lisp-indent-hook #'lisp-indent-function)

;;; xyzzy.el ends here
