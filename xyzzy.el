;;; xyzzy.el --- CommonLisp/SLIME/xyzzyの関数/変数をEmacsで使うためのライブラリ

;; Copyright (C) 2009,2010 Shigeru Kobayashi

;; Author: Shigeru Kobayashi <shigeru.kb@gmail.com>
;; Version: 0.1
;; Created: 2009-03-11
;; Keywords: lisp,extensions
;; URL: http://github.com/kosh04/emacs-lisp/raw/master/xyzzy.el

;; This file is NOT part of Emacs.

;;; Commentary:

;; これはなに？
;; ------------
;;
;; 亀井さん作成のEmacsクローンxyzzyと本家emacsの関数・変数名等の
;; 違いを吸収するためのライブラリみたいなものです。
;;
;; 何のために使うの？
;; ------------------
;;
;; - 動作は同じで、名前が微妙に異なる関数・変数をエイリアスにして
;;   とりあえず使えるようにする
;;   例: delete-hook[emacs] と remove-hook[xyzzy]
;;
;; - xyzzyにしかない関数・変数をemacsでも使えるように
;;   例: kill-scratch-hook, sub-directory-p
;;
;; また、CommonLisp/SLIMEの関数群も少ないですが定義してあります。

;;; Installation:

;; 1. このファイルをパスの通ったディレクトリに置く
;; 2. 必要ならばバイトコンパイルする
;;    `emacs -batch -f batch-byte-compile xyzzy.el`
;; 3. .emacs に次の行を加える
;;    (require 'xyzzy)

;;; ChangeLog:

;; - 2010-10-09
;;   Emacs21でもとりあえずバイトコンパイルできるように関数やシンタックスを修正
;; - 2009-04-19
;;   初版作成

;;; Code:

(provide 'xyzzy)

(eval-when-compile (require 'cl))
;; (require 'ielm)

;;; @@Data-Type
(fset 'compiled-function-p #'byte-code-function-p)

(defun alpha-char-p (char)
  (integerp (string-match "[A-Za-z]" (char-to-string char))))

(defun alphanumericp (char)
  (integerp (string-match "[A-Za-z0-9]" (char-to-string char))))

(defun kanji-char-p (character)
  (multibyte-string-p (char-to-string character)))

;;; @@Variable-and-Constant
(defvaralias '*modules* 'features)
(defvaralias '*load-path* 'load-path)
(defvaralias '*pre-startup-hook* 'before-init-hook) ; emacs-startup-hook ?
(defvaralias '*post-startup-hook* 'after-init-hook) ; window-setup-hook ?
(defvaralias 'si:*command-line-args* 'command-line-args-left) ; command-line-args ?
(defvaralias '*load-pathname* 'load-file-name)
(defvaralias '*etc-path* 'data-directory)
;;            引数有りで実行            引数なしで実行される
(defvaralias '*query-kill-buffer-hook* 'kill-buffer-query-functions)
(defvaralias '*query-kill-xyzzy-hook* 'kill-emacs-query-functions)

(fset 'defconstant #'defconst)

(defmacro defparameter (symbol value &optional docstring)
  `(if (boundp ',symbol)
       (progn
         (set ',symbol ,value)
         ,(if docstring
              `(put ',symbol 'variable-documentation ,docstring))
         ',symbol)
       (defvar ,symbol ,value ,docstring)))

;;; 代入する変数が束縛されているかどうかは実行時まで分からない
;; (defmacro defparameter (symbol value &optional doc-string)
;;   (if (boundp symbol)
;;       `(progn
;;          (set ',symbol ,value)
;;          ,(if doc-string
;;               `(put ',symbol 'variable-documentation ,doc-string))
;;          ',symbol)
;;       `(defvar ,symbol ,value ,doc-string)))

(defmacro defvar-local (symbol value &optional docstring)
  `(progn
     (defvar ,symbol ,value ,docstring)
     (make-variable-buffer-local ',symbol)))

;; cl-macs.el:1716
;;; Some more Emacs-related place types.
;; (defsetf ...)

;;; @@制御構造

;; Common Lisp `tagbody' for Emacs Lisp
;; http://www.emacswiki.org/emacs/tagbody.el

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
(defun find-symbol (string)
  (intern-soft string))

;; symbol-function ~= indirect-function

;;; @@Number
(fset 'rem #'%)
(defun logandc1 (x y) (logand (lognot y) y))
(defun logandc2 (x y) (logand x (lognot y)))

;; from Corman Lisp
;; http://www.cormanlisp.com/CormanLisp/patches/2_5/math2.lisp
(defun logcount (integer)
  (check-type integer integer)
  ;; if negative, use two's complement to flip
  (do ((x (if (< integer 0) (- (+ integer 1)) integer) (ash x -1))
       (count 0 (+ count (logand x 1))))
      ((= x 0) count)))

(defun integer-length (integer)
  (check-type integer integer)
  (do ((x (if (< integer 0) (- (+ integer 1)) integer) (ash x -1))
       (count 0 (1+ count)))
      ((= x 0) count)))

(defun logbitp (index integer)
  (check-type index (integer 0 *))
  (check-type integer integer)
  (< 0 (logand integer (expt 2 index))))

;;; @@Character
(fset 'char #'elt)
(fset 'char-columns #'char-width)

;; [xyzzy] C-q 2 1     -> ! (#x21)
;; [emacs] C-q 2 1 RET -> ^Q (#o21)
(defun quote-char (&optional arg)
  (interactive "*p")
  (let ((read-quoted-char-radix 16))
    (quoted-insert arg)))

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

;; FIXME: もうちょっと綺麗にならないものか
(defun both-case-p (char)
  (let ((case-fold-search t)
        (uc (upcase char))
        (dc (downcase char)))
    (and (char-equal uc dc)
         (/= uc dc))))

(defun upper-case-p (char)
  (let ((case-fold-search nil))
    (and (both-case-p char)
         (char-equal char (upcase char)))))

(defun lower-case-p (char)
  (and (both-case-p char)
       (not (upper-case-p char))))

;; use Mule-UCS (Emacs 21)
;; Emacs23ならば内部コードがunicodeなので必要無し(あっても問題ない)
;; Emacs20ならばmultibyte-char-to-unibyteも一応代用できそう
(defun char-unicode (char) (encode-char char 'ucs))
(defun unicode-char (code) (decode-char 'ucs code))

;; (fset 'char-int #'identity)

;; `describe-char-unicodedata-file' を設定していれば利用可能
;; Help > describe-variable > describe-char-unicodedata-file
(autoload 'describe-char-unicode-data "descr-text"
  "Return a list of Unicode data for unicode CHAR.")
(defun char-name (character)
  (nth 1 (assoc "Name" (describe-char-unicode-data character))))

;; (defun name-char (name) )

(defun char= (char &rest more-chars)
  (let ((case-fold-search nil))
    ;; (every (lambda (c) (char-equal c char)) more-chars)
    (do* ((chars more-chars (cdr chars))
          (c #1=(car chars) #1#))
         ((null chars) t)
      (if (not (char-equal c char))
          (return nil)))
    ))

;;; @@Sequence
(defun remove-trail-slash (str)
  "STR の末尾に `/' があれば削除した文字列を返す。"
  (cond ((string= "" str) str)
        ((string= "/" (substring str -1))
         (substring str 0 -1))
        (t str)))

(defun append-trail-slash (str)
  (cond ((string= "" str) "/")
        ((string= "/" (substring str -1)) str)
        (t (concat str "/"))))

;; (defun map-slash-to-backslash (string) (replace-regexp-in-string "/" "\\\\" string))
;; (defun map-backslash-to-slash (string) (replace-regexp-in-string "\\\\" "/" string))
(defun map-slash-to-backslash (string) (subst-char-in-string ?/ ?\\ string))
(defun map-backslash-to-slash (string) (subst-char-in-string ?\\ ?/ string))

;; 大文字小文字を区別して文字列の比較
;; string-equalのエイリアスとして定義されている(subr.el)ので上書きすると問題があるかも
;; (defun* string= (string1 string2 &key (start1 0) end1 (start2 0) end2)
;;   (check-type string1 string)
;;   (check-type string2 string)
;;   (eq (compare-strings string1 start1 end1
;;                        string2 start2 end2
;;                        nil)
;;       t))

;; (defun* string/= (string1 string2 &key (start1 0) end1 (start2 0) end2)
;;   (not (string= string1 string2
;;                 :start1 start1 :end1 end1
;;                 :start2 start2 :end2 end2)))

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
  ;; (setq char-bag (coerce char-bag 'list))
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

(defun* substitute-string (string pattern replacement
                                  &key case-fold start end skip count)
  (let ((case-fold-search case-fold))
    (replace-regexp-in-string pattern replacement string 'fixedcase)))

(defun* parse-integer (string &key start end radix junk-allowed)
  (declare (ignore junk-allowed))
  (string-to-number (substring string (or start 0) end) radix))

;;; @@List
(fset 'safe-car #'car-safe)
(fset 'safe-cdr #'cdr-safe)

;;; @@Hash
;;; @@Array
;;; @@Chunk

;;; @@Eval
(defadvice eval-last-sexp (before eval-safe activate)
  "ポイントがシンボルの途中でもエラーにならない eval-last-sexp"
  (with-syntax-table emacs-lisp-mode-syntax-table
    (skip-syntax-forward "w_")))
;; (ad-deactivate 'eval-last-sexp)

;; Emacsのeval-regionの返り値が常にnil、出力はデフォルトでは破棄される
;; -> せめて出力は破棄せずにミニバッファに表示したい
;; できれば返り値も弄りたいが...
(defadvice eval-region (before output-to-stdout activate)
  ;; Programs can pass third argument PRINTFLAG which controls output:
  ;; A value of nil means discard it; anything else is stream for printing it.
  (or (ad-get-arg 2)
      (ad-set-arg 2 standard-output)))

;; (defvaralias 'hoge 'load-in-progress)

;; * Emacsは文字コードに関係なくファイルをロードできるはず
;; (defun  mc-load-file (filename &optional encoding)
;;   (load-with-code-conversion filename filename t))
;; set-auto-coding-for-load

;;; @@Input/Output
(defun princ-to-string (object &optional stream)
  (with-output-to-string (princ object stream)))

;; (apropos "network")

;; (defun* xyzzy-open-network-stream
;;     (buffer host service &key incode outcode eol-code)
;;   (declare (ignore eol-code))
;;   (make-network-process :buffer buffer
;;                         :host host
;;                         :service service
;;                         :codng `(incode . outcode)))
;; (defalias 'open-network-stream-xyzzy 'xyzzy-open-network-stream)

;; (values)を使うpprintは実装できない？

;;; @@Filesystem
(fset 'merge-pathnames #'expand-file-name)
(fset 'file-namestring #'file-name-nondirectory)
(fset 'directory-namestring #'file-name-directory)
(fset 'get-buffer-file-name #'buffer-file-name)
(fset 'file-exist-p #'file-exists-p)
(fset 'pathname-type #'file-name-extension)
(fset 'find-other-file  #'find-alternate-file)
(fset 'get-file-attributes #'file-attributes)
(fset 'make-temp-file-name #'make-temp-file)

;; (defun user-homedir-pathname () default-directory)
;; get-disk-usage

;; CLにファイルのリンク先を参照する関数はあるのか？
;; file-truename, file-chase-links
(defun* directory (pathname &key absolute recursive wild depth file-only
                            show-dots count directory-only callback file-info)
  (declare (ignore recursive depth show-dots count))
  (labels ((filter (f seq)
             (let (acc)
               (dolist (x seq acc)
                 (if (funcall f) x)))))
    (let (files)
      (dolist (file (if file-info
                        (directory-files-and-attributes pathname absolute wild)
                        (directory-files pathname absolute wild)))
        (if (cond (file-only
                   (and (file-exists-p file)
                        (not (file-directory-p file))))
                  (directory-only
                   (file-directory-p file))
                  (t t))
            (push (funcall (if callback callback #'identity)
                           file)
                  files)))
      (nreverse files))))
;; (directory ".")

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
  (let ((dir (delete "" (split-string (file-name-directory
                                       (expand-file-name pathname))
                                      "/"))))
    (if (memq system-type '(ms-dos windows-nt)) ; ? cygwin
        (cdr dir)                               ; ignore drive letter
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

;; (defun sub-directory-p (dir parent)
;;   (let ((case-fold-search t))
;;     (string-match (concat "^" (regexp-quote
;;                                (file-name-as-directory parent)))
;;                   (file-name-as-directory dir))))

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

;; (byte-compile-dest-file "xyzzy.el")    => "xyzzy.elc"
;; (byte-compile-dest-file "xyzzy.el.gz") => "xyzzy.elc"
(defun compile-file-pathname (pathname)
  (require 'bytecomp)
  (byte-compile-dest-file pathname))

(defun file-length (pathname)
  "Return PATHNAME size in bytes."
  (nth 7 (file-attributes pathname)))

;;; @@Error
(fset 'plain-error #'error)

;;; @@Window
(fset 'set-window #'select-window)
(fset 'next-page #'scroll-up)
(fset 'previous-page #'scroll-down)

;; split-window-vertically
;; Emacs: ウィンドウを上下に分割 [C-x 2]
;; xyzzy: ウィンドウを左右に分割 [C-x 5]

(defun move-previous-window (&optional arg)
  (interactive "p")
  (other-window (- arg)))

(defun get-window-start-line ()
  ;; どちらも同じ
  (or ;; (line-number-at-pos (window-start))
     (save-excursion
       (move-to-window-line 0)
       (1+ (count-lines (point-min) (point))))
     ))

;; (= (save-excursion (move-to-window-line -1) (line-number-at-pos))
;;    (line-number-at-pos (window-end)))   ; => nil

(defun get-window-line (&optional window)
  "ウィンドウのカーソルの表示行を返します. [zero-origin]"
  ;; posn-actual-col-row ?
  (cdr (posn-col-row (posn-at-point (point) window)))
  ;; ポイント位置によって値がまちまちなので利用できない
  ;; (count-lines (save-excursion
  ;;                (move-to-window-line 0)
  ;;                (point))
  ;;              (point))
  )

;; FIXME:
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
  (ignore-errors (scroll-up arg)))

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

(put 'scroll-left 'disabled nil)        ; C-x <
(put 'scroll-right 'disabled nil)       ; C-x >

(fset 'toggle-ruler #'ruler-mode)
(fset 'toggle-cursor-line #'global-hl-line-mode)
;; 下線カーソルface変更
;; (setq hl-line-face 'underline)
;; or (toggle-truncate-lines arg)
(defun set-buffer-fold-type-none () (interactive) (setq truncate-lines t))
(defun set-buffer-fold-type-window () (interactive) (setq truncate-lines nil))
(defun toggle-eof (&optional arg)
  (interactive "P")
  (setq default-indicate-empty-lines
        (or arg (not default-indicate-empty-lines))))

;;; @@Buffer
(fset 'selected-buffer #'current-buffer)
(fset 'find-buffer #'get-buffer)
(fset 'create-new-buffer #'generate-new-buffer)
(fset 'buffer-can-undo-p #'buffer-enable-undo)
(fset 'kill-selected-buffer #'kill-this-buffer)
(fset 'buffer-process #'get-buffer-process)

;; *DON'T* use Emacs23 or later
;; (defvaralias 'buffer-name 'major-mode)

;; (get-next-buffer)

(defun delete-buffer (buffer)
  "Kill the BUFFER without query."
  (let ((kill-buffer-query-functions nil))
    (kill-buffer buffer)))

(defun deleted-buffer-p (buffer)
  (not (buffer-live-p buffer)))

(defun file-visited-p (&optional buffer)
  (stringp (buffer-file-name buffer)))

;; (window-buffer-height (selected-window)) だとEOFのみの行を認識してくれない
(defun buffer-lines (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (line-number-at-pos (point-max))))

;; revert-buffer{,-with-coding-system}
(defun xyzzy-revert-buffer (&optional encoding)
  (interactive (list (if current-prefix-arg
                         (read-coding-system "Encoding: ")
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

(defun enum-buffers (fn)
  "関数FNがnon-nilを返すまでバッファを列挙し続けます."
  (let (ret)
    (catch 'found
      (dolist (buffer (buffer-list))
        (setq ret (funcall fn buffer))
        (if ret (throw 'found ret))))))

;; 0:LF(unix) 1:CRLF(dos) 2:CR(mac)
(defconst buffer-eol-alist '((0 . unix) (1 . dos) (2 . mac)))
;;(defvar *default-eol-code* 1)

;; バッファのエンコードで改行コードが指定されてない場合はある？
;; -> ファイルに関連付けされていないバッファ
(defun buffer-eol-code (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (coding-system-eol-type buffer-file-coding-system)))

(defun change-eol-code (&optional arg)
  (interactive "P")
  (let ((eol-type (or (and arg (prefix-numeric-value arg))
                      (buffer-eol-code))))
    (when (integerp eol-type)
      (set-buffer-file-coding-system (cdr (assoc (mod (1+ eol-type) 3)
                                                 buffer-eol-alist))
                                     nil 'nomodify))))

(defun xyzzy-erase-buffer (buffer)
  (with-current-buffer (or buffer (current-buffer))
    (let ((buffer-read-only nil))
      (erase-buffer)                      ; バッファ上のテキスト,restriction
      ;; ・修正日付
      (set-buffer-modified-p nil)         ; 変更フラグ
      (setq buffer-undo-list nil)         ; UNDO情報
      )))

;; (put 'erase-buffer 'disabled nil)

;; change-default-fileio-encoding
;; change-default-eol-code
(fset 'change-clipboard-encoding #'set-clipboard-coding-system)
(fset 'change-fileio-encoding #'set-buffer-file-coding-system)

;; ? next-selection-coding-system
(defvaralias '*clipboard-char-encoding* 'selection-coding-system
  "クリップボードエンコーディング")

;; optporop.l backup.l
(defvaralias 'make-backup-file-always 'delete-old-versions
  "番号が最後までいったら勝手に詰める") ; ?

;; [emacs] (buffer-local-value VARIABLE BUFFER)
;; [xyzzy] (buffer-local-value BUFFER SYMBOL)
(defun xyzzy-buffer-local-value (buffer symbol)
  (buffer-local-value symbol buffer))

(defun set-buffer-file-name (filename &optional buffer)
  "BUFFERに関連付けられているファイル名をFILENAMEに変更します."
  (with-current-buffer (or buffer (current-buffer))
    (setq buffer-file-name filename)))

(defun buffer-fileio-encoding (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    buffer-file-coding-system))

(defun set-buffer-fileio-encoding (code &optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (set-buffer-file-coding-system code)))

;; clear-undo-boundary

;; (list make-backup-files version-control kept-old-versions kept-new-versions 'pack-backup-file-name make-backup-file-always)
;; ediff の独立ウィンドウを参考に (select-buffer) [f2]

;;; @@Minibuffer

;;; @@Region
(defadvice kill-ring-save (after kill-ring-msg activate)
  ;; kill-new の方がいいのかな?
  (message "Region copied"))

;; string-rectangle (C-x r t) が quote-region の代用になるかも

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
;; (defadvice kill-line (before kill-line-read-only activate)
;;   (barf-if-buffer-read-only))

;;; 読み込み専用のテキストをキル
;;; http://www.bookshelf.jp/cgi-bin/goto.cgi?file=meadow&node=kill-read-only-ok
;; このオプションがnil以外であると, kill-regionは, バッファが読み出し
;; 専用であってもエラーとしない. そのかわりに, キルリングを更新しバッ
;; ファは変更せずに戻る.
;;
;; エラーが出るかそうでないかの違いだけで、結局コピーは出来てしまうんだが...
;; (setq kill-read-only-ok t)

(put 'upcase-region 'disabled nil)      ; C-x C-u
(put 'downcase-region 'disabled nil)    ; C-x C-l
(put 'narrow-to-region 'disabled nil)   ; C-x n n

(eval-when-compile
  (require 'japan-util))

;; japanese-katakana                       ; ひらがな→カタカナ
;; japanese-hiragana                       ; カタカナ→ひらがな
;; japanese-hankaku                        ; 全角→半角
;; japanese-zenkaku                        ; 半角→全角

(defun* map-to-half-width-region (from to &key ascii hiragana katakana greek cyrillic)
  "指定したリージョンを半角文字に変換します."
  (interactive "*r")
  (japanese-hankaku-region from to ascii))
;; (fset 'map-to-half-width-region #'japanese-hankaku-region)

;; FIXME: defun*-interactive を組み合わせるとインタラクティブに呼び出せない？
(defun* map-to-full-width-region (from to &key ascii hiragana katakana greek cyrillic)
  "指定したリージョンを全角文字に変換します."
  (interactive "*r")
  (japanese-zenkaku-region from to katakana))
;; (fset 'map-to-full-width-region #'japanese-zenkaku-region)

(defun* map-to-half-width-string (string &key ascii hiragana katakana greek cyrillic)
  "文字列STRINGを半角に変換します."
  (japanese-hankaku string ascii))

(defun* map-to-full-width-string (string &key ascii hiragana katakana greek cyrillic)
  "文字列STRINGを全角に変換します."
  (japanese-zenkaku string))

;;; @@Mode
(fset 'delete-hook 'remove-hook)
(fset 'run-hook-with-args-while-success 'run-hook-with-args-until-failure)
;; #'toggle-mode は便利か？

;;; @@Syntax
(defparameter syntax-char-alist
  '(
    ;; emacs/xyzzy共通
    (?\  . whitespace-syntax)           ; ?\s
    (?w  . word-constituent)
    (?_  . symbol-constituent)
    (?.  . punctuation)
    (?\( . open-parenthesis)
    (?\) . close-parenthesis)
    (?\" . string-quote)
    (?\\ . escape)
    (?/  . character-quote)
    (?$  . paired-delimiter)            ; [math]
    (?\' . expression-prefix)           ; [symbol-prefix]
    (?<  . comment-starter)
    (?>  . comment-ender)
    (?@  . inherit-standard-syntax)
    (?!  . generic-comment-fence)
    (?|  . generic-string-fence)
    ;; Flags
    (?1 . start-of-multi-comment1)
    (?2 . start-of-multi-comment2)
    (?3 . end-of-multi-comment1)
    (?4 . end-of-multi-comment2)
    (?b . comment-sequence-b)
    (?n . nestable-comment-sequence)
    (?p . prefix-character)
    ;; xyzzy専用？
    (?\" . string-quote)
    (?x . junk)
    (?\{ . open-tag)
    (?\} . close-tag)
    ))

(eval-when-compile
  (defmacro define-syntax-xxx-p (name c)
    `(defun ,(intern (format "syntax-%s-p" (symbol-name name)))
         (char &optional syntax-table)
       (with-syntax-table (or syntax-table (syntax-table))
         (eql (char-syntax char) ,c)))))

;; ちゃんと並び替えよう
;; /* 2文字コメントどうする？ */
;; http://www.fan.gr.jp/~ring/doc/elisp_20/elisp_35.html
(define-syntax-xxx-p whitespace ?\ )    ; ?\s
(define-syntax-xxx-p word ?w)
(define-syntax-xxx-p symbol ?_)
(define-syntax-xxx-p punctuation ?.)
(define-syntax-xxx-p open ?\()
(define-syntax-xxx-p close ?\))
(define-syntax-xxx-p character-quote ?\")
(define-syntax-xxx-p escape ?\\)
;; character-quote (`/')
(define-syntax-xxx-p paired-delimiter ?$)
(define-syntax-xxx-p expression-prefix ?\')
(define-syntax-xxx-p start-comment ?<)
(define-syntax-xxx-p end-comment ?>)
;; inherit standard syntax (`@')
;; generic string fence (`|')
;; generic comment fence (`!')
(define-syntax-xxx-p string-quote ?\")      ; string
(define-syntax-xxx-p junk ?x)
(define-syntax-xxx-p open-tag ?\{)
(define-syntax-xxx-p close-tag ?\})

;; FIXME: シンタックスのエントリをマージできないからあんまり使えない
;; Emacsは1文字ごとにまとめて構文定義しないと駄目

;; (defmacro define-set-syntax-xxx (name newentry)
;;   `(defun ,(intern (format "set-syntax-%s" name)) (chare &optional syntax-table)
;;      (modify-syntax-entry char ,newentry syntax-table)))

;; (info "(elisp) Syntax Tables")
;; (char-syntax)
;; (describe-syntax)
;; (describe-char (point))
;; (syntax-after (point))
;; (get-char-property (point) 'syntax-table)
;; (internal-describe-syntax-value (aref (syntax-table) ?\s))=>which means: whitespace
;; set-char-table-extra-slot

(defun set-syntax-match (syntax-table open-char close-char)
  (modify-syntax-entry open-char (format "(%c" close-char) syntax-table)
  (modify-syntax-entry close-char (format ")%c" open-char) syntax-table))

(defun set-syntax-start-multi-comment (syntax-table string)
  (modify-syntax-entry (elt string 0) ". 1" syntax-table)
  (modify-syntax-entry (elt string 1) ". 2" syntax-table))

(defun set-syntax-end-multi-comment (syntax-table string)
  (modify-syntax-entry (elt string 0) ". 3" syntax-table)
  (modify-syntax-entry (elt string 1) ". 4" syntax-table))

;; ? parse-sexp-ignore-comments
(defun set-syntax-start-c++-comment (syntax-table char &optional parse-sexp-ignore-comment-p)
  (declare (ignorable parse-sexp-ignore-comment-p))
  (modify-syntax-entry char "< 12b" syntax-table))

(defun set-syntax-end-c++-comment (syntax-table char &optional parse-sexp-ignore-comment-p)
  (modify-syntax-entry char "> 4b" syntax-table))

(defun* use-syntax-table (syntax-table &optional buffer (invalidate-p t))
  (declare (ignorable invalidate-p))
  (with-current-buffer (or buffer (current-buffer))
    (set-syntax-table syntax-table)))

;; lisp/expand.el を参考に
(defun parse-point-syntax (&optional point)
  (or point (setq point (point)))
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
    (dolist (x tmp) (add-to-list acc x))
    (setq tmp (where-is-internal command local-keymap))
    (dolist (x tmp) (add-to-list acc x))
    (when minor-mode-keymaps
      (setq tmp (where-is-internal command global-keymap))
      (dolist (x tmp) (add-to-list acc x)))
    (nreverse acc)))

(defun get-ime-mode ()
  (stringp current-input-method))

;; (defun toggle-ime ()
;;   (interactive)
;;   ;; (mw32-ime-toggle)
;;   (toggle-input-method))
(fset 'toggle-ime 'toggle-input-method)

;;; @@Text
(defun convert-encoding-to-internal (encoding input-string &optional output-stream)
  "convert string INPUT-STRING (internal -> ENCODING)"
  (declare (ignore output-stream))
  (encode-coding-string input-string encoding))

(defun convert-encoding-from-internal (encoding input-string &optional output-stream)
  "convert string INPUT-STRING (ENCODING -> internal)"
  (declare (ignore output-stream))
  (decode-coding-string input-string encoding))

(defun map-utf8-to-internal (input-string &optional output-stream)
  (convert-encoding-to-internal 'utf-8 input-string output-stream))

(defun map-internal-to-utf-8 (input-string &optional output-stream)
  (convert-encoding-from-internal 'utf-8 input-string output-stream))

(defun map-char-encoding-region (p1 p2 &optional encoding)
  (interactive (list (region-beginning)
                     (region-end)
                     (if current-prefix-arg
                         (read-non-nil-coding-system "Encoding: "))))
  (barf-if-buffer-read-only)
  (check-coding-system encoding)
  (decode-coding-region p1 p2 encoding))

(fset 'detect-char-encoding #'detect-coding-string) ; ?

;; クリップボードエンコーディング
;; (set-selection-coding-system)

;; (defun do-completion (from to type &optional compl word last-char) )

(defun set-tab-columns (column &optional buffer)
  (if (bufferp buffer)
      (set 'tab-width column)
      (set-default 'tab-width column)))

;;; @@Search, Regexp
(fset 'ed::protect-match-data 'save-match-data)
(fset 'store-match-data #'set-match-data)

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
(defun y-or-n-p-with-dialog (fmt &rest args)
  (let ((last-nonmenu-event nil)
        (use-dialog-box t))
    (y-or-n-p (apply #'format fmt args))))

;;; @@Date-Time
;; Common Lisp (xyzzy) features `universal-time'
;; Emacs Lisp          features `UNIX-time'
;; (fset 'get-universal-time #'current-time)
;; (fset 'encode-universal-time #'encode-time)
;; (fset 'decode-universal-time #'decode-time)
(fset 'format-date-string #'format-time-string)
(fset 'get-internal-real-time #'get-internal-run-time) ; ?
(defun get-decoded-time () (decode-time (current-time)))

;; ? タイマーの書式 [...] を調べよう
(defun start-timer (interval fn &optional one-shot-p)
  (run-at-time t (if one-shot-p nil interval) fn))
(fset 'stop-timer #'cancel-function-timers)
(defun stop-all-timers () (mapc #'cancel-timer timer-list))

;;; @@Menu
;; http://www.bookshelf.jp/cgi-bin/goto.cgi?file=meadow&node=mouse%20click
(defun bingalls-edit-menu (event)
  "右クリックでメニュー"
  (interactive "e")
  (popup-menu menu-bar-edit-menu))

;;; @@Filer
;;; filer [xyzzy] <-> dired [emacs]
(eval-when-compile
  (require 'dired))

(defvaralias 'filer-keymap 'dired-mode-map)

(fset 'filer-mark-path-mask #'dired-mark-files-regexp) ; % m

(defun filer-get-current-file ()
  (dired-get-filename nil 'no-error))

;;; @@Position
(fset 'marker-point #'marker-position)

;; (defun goto-marker (marker)
;;   "マーカーのポジションへポイントを移動します。"
;;   (move-marker marker (point)))

(defun goto-matched-parenthesis ()
  "Go to the matching parenthesis if on parenthesis."
  (interactive)
  (cond ((looking-at "[([{]")
         (forward-sexp) (backward-char))
        ((looking-at "[])}]")
         (forward-char) (backward-sexp))
        (t nil)))

(defun current-line-number ()
  (line-number-at-pos (point)))

(defun goto-bol ()
  (or (beginning-of-line 1)
      (goto-char (line-beginning-position))))

(defun goto-eol ()
  (or (end-of-line 1)
      (goto-char (line-end-position))))

;; (put 'set-goal-column 'disabled nil)

;;; @@Process

;; (xyzzy:call-process CMD :wait t)
;; == (emacs:call-process PROGNAME nil 0 nil ARGS)
;; BUFFER; 0 means discard output and don't wait

(defmacro with-setenv (environ &rest body)
  `(let ((process-environment (copy-sequence process-environment)))
     (mapc (lambda (x)
             (setenv (car x) (cdr x)))
           ,environ)
     ,@body))

;; 子プロセスの生成
;; start-process[emacs] <-> make-process[xyzzy]
(defun* make-process (cmd-line &key environ output exec-directory
                               incode outcode eol-code)
  (declare (ignore eol-code))
  (setq cmd-line (split-string cmd-line " "))
  (let* ((default-directory (or exec-directory default-directory))
         (program (car cmd-line))
         (args (cdr cmd-line))
         (proc (apply #'start-process "xyzzy-process"
                      (or output (current-buffer))
                      program args)))
    (set-process-coding-system proc
                               (or incode (car default-process-coding-system))
                               (or outcode(cdr default-process-coding-system)))
    proc))

;; decoding=incode;encoding=outcode
(defun process-incode (process) (car (process-coding-system process)))
(defun process-outcode (process) (cdr (process-coding-system process)))
(defun set-process-incode (process code) (set-process-coding-system process nil code))
(defun set-process-outcode (process code) (set-process-coding-system process code nil))

;; NOT Tested
(defun execute-subprocess (cmd &optional arg bufname environ directory)
  (interactive "s& ")
  (let ((default-directory (or directory default-directory)))
    (with-setenv environ
      (shell-command cmd bufname shell-command-default-error-buffer))))

;; (defun launch-application (cmd)
;;   (start-process "launch-application" nil shell-file-name cmd))
;; (ed::shell-command-line ...) コマンドライン処理関数が欲しいところ
(when (fboundp 'w32-shell-execute)
(defun launch-application (app)
  "外部プログラムを実行します."
  (interactive "s%% ")
  (let ((w32-start-process-show-window t)) ; ?
    (w32-shell-execute "open" app)))

(defun shell-execute (filename &optional directory params)
  "FILENAMEを関連付けられたプログラムで起動する."
  ;; $ cmd /c start http://www.google.com/
  ;; (call-process "cmd.exe" nil 0 nil "\\/c" "start" FILENAME)
  (w32-shell-execute "open" (if (file-exists-p filename)
                                (expand-file-name filename)
                                filename)))
;; (defvar *eshell* shell-file-name)
(defun run-console ()
  (interactive)
  (launch-application (or (and (boundp 'explicit-shell-file-name)
                               (symbol-value 'explicit-shell-file-name))
                          (getenv "ESHELL")
                          shell-file-name)))
)                                  ; end of `(fboundp 'w32-shell-execute)'

;; (fset 'process-exit-code #'process-exit-status)

(defun filter-region (command &optional start end)
  (interactive "*s| \nr")
  (shell-command-on-region start end command nil 'replace))

(defun filter-buffer (command)
  (interactive "*s# ")
  (filter-region command (point-min) (point-max)))

;; shell-command.elがあればミニバッファからのコマンド補完が便利になる
;; http://namazu.org/~tsuchiya/elisp/shell-command.el
;; (require 'shell-command)
;; (featurep 'shell-command)
(eval-after-load "shell-command"
'(progn
  (defun filter-region (command &optional start end)
    (interactive (progn
                   (barf-if-buffer-read-only)
                   (unless (mark)
                     (error "The mark is not set now, so there is no region"))
                   (list (shell-command-read-minibuffer
                          "| " default-directory
                          nil nil nil 'shell-command-history)
                         (region-beginning)
                         (region-end))))
    (shell-command-on-region start end command nil 'replace))

  (defun filter-buffer (command)
    (interactive (progn
                   (barf-if-buffer-read-only)
                   (list (shell-command-read-minibuffer
                          "| " default-directory
                          nil nil nil 'shell-command-history))))
    (filter-region command (point-min) (point-max)))
  ))

;;; @@System
(defun find-load-path (filename)
  (locate-library filename))

(fset 'machine-name #'system-name)
(fset 'user-name #'user-login-name)

;; src/w32fns.c, lisp/w32-fns.el
(when (fboundp 'w32-version)
  (defun os-major-version () (nth 0 (w32-version)))
  (defun os-minor-version () (nth 1 (w32-version)))
  (defun os-build-number () (nth 2 (w32-version))))

(if (fboundp 'w32-get-clipboard-data)
    (fset 'get-clipboard-data #'w32-get-clipboard-data)
    (fset 'get-clipboard-data #'x-get-clipboard))

;; (apropos "clipoboard")

;; FIXME: フォーカスは移動するがウィンドウが最前面にこない気がする
(defun si:*show-window-foreground ()
  "ウィンドウを最前面に表示."
  ;; not known to be defined.
  (if (and (fboundp 'x-focus-frame)
           (eq window-system 'x))
      (x-focus-frame (selected-frame)))
  (raise-frame (selected-frame))
  (frame-focus (selected-frame)))

;; (fset 'dump-xyzzy #'dump-emacs)
;; si:dump-image-path

(defun start-xyzzy-server () (server-start))
(defun stop-xyzzy-server () (server-start t))

;; @@Condition
;; si:*print-condition
(fset 'si:*condition-string #'error-message-string)

(defsubst find-condition-variable (handlers)
  (let (e)
    (dolist (handler handlers)
      (pushnew (car (nth 1 handler)) e :test #'equal))
    ;; condition-case のエラー用変数は1つだけなので注意を促す
    (if (/= 1 (length e)) (warn "plural condition variable are not allowed: %s" e))
    (car e)))

(defmacro handler-case (form &rest cases)
  `(condition-case ,(find-condition-variable cases)
       ,form
     ,@(mapcar (lambda (c)
                 ;; (error (e) form1 ...) => (error form1 ...)
                 (cons (car c) (nthcdr 2 c)))
               cases)))

;;; @@Misc
(fset 'modulep #'featurep)
(fset 'gc #'garbage-collect)
(fset 'msgbox #'message-box)
(defun etc-path () data-directory)

;; tail-f.l
(defun tail-f (filename)
  (interactive "ftail-f: ")
  (find-file filename)
  (turn-on-auto-revert-tail-mode))

(fset 'quit-recursive-edit #'abort-recursive-edit) ; ? exit-recursive-edit
;; (fset 'toggle-trace-on-error #'toggle-debug-on-error)

;; (fset 'documentation #'documentation-property)

(defun goto-column (column)
  (move-to-column column nil))

(defun si:base64-encode (string &optional output-stream)
  (cond (output-stream
         (print #1=(base64-encode-string (encode-coding-string string 'binary))
                output-stream)
         t)
        (t #1#)))

(defun si:base64-decode (string &optional output-stream fold-width)
  (declare (ignore fold-width))
  (cond (output-stream
         (print #1=(decode-coding-string (base64-decode-string string) 'emacs-mule)
                output-stream)
         t)
        (t #1#)))

(defsubst make-unreserved-chars (literal-char)
  "引数文字列からURLエンコードしない文字のリストを作成します."
  (if (string-equal literal-char "")
      nil
      (let ((words (concat "[^" literal-char "]"))
            (acc '()))
        (dolist (c (number-sequence 0 127))
          (if (string-match words (char-to-string c))
              (push c acc)))
        (nreverse acc))))

;; (set-difference (make-unreserved-chars "-A-Za-z0-9$_.+!*'(|),") url-unreserved-chars) => (?| ?, ?+ ?$)

;; url/url-util.el
(defun si:www-url-encode (string &optional output-stream literal-char)
  "STRINGをURLエンコードします."
  (declare (ignore output-stream))
  (require 'url-util)
  (let ((url-unreserved-chars
         (if (eq literal-char 't)
             nil
             (make-unreserved-chars (or literal-char "-A-Za-z0-9$_.+!*'(|),")))))
    (url-hexify-string (encode-coding-string string locale-coding-system))))

;; url-unhex-string
(defun si:www-url-decode (input-string &optional output-stream)
  "STRINGをURLデコードします."
  (declare (ignore output-stream))
  (require 'url-util)                   ; use `replace-regexp-in-string'
  (let ((decoded-url
         (replace-regexp-in-string "%\\([0-9A-Fa-f][0-9A-Fa-f]\\)"
                                   #'(lambda (str)
                                       ;; "%FA" => "\xFA"
                                       (char-to-string
                                        (string-to-number (match-string 1 str) 16)))
                                   input-string)))
    (decode-coding-string decoded-url locale-coding-system)))

;; もしくは urlencode.el
;; "http://taiyaki.org/elisp/urlencode/"
;; "http://taiyaki.org/elisp/urlencode/src/urlencode.el"

(defun update-mode-line (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (if (fboundp 'mw32-ime-mode-line-update)
        (mw32-ime-mode-line-update)
        (force-mode-line-update)
        )))

(defun refresh-screen (&optional f)
  (redraw-display)
  (redraw-frame (selected-frame)))

(defun autoload-function-p (def)
  (eq (car-safe (symbol-function def)) 'autoload))

(defvaralias '*expected-fileio-encoding* 'coding-system-for-read
  "エンコードを指定してファイルの読み込み")
;; buffer-file-coding-system-explicit

(fset 'char-encoding-p #'coding-system-p)

(defsubst valid-buffer-list ()
  (let (acc)
    (dolist (b (buffer-list))
      (unless (string-match "^ " (buffer-name b))
        (push b acc)))
    (nreverse acc)))

(defun xyzzy-grep (regexp &optional arg)
  "バッファ内を正規表現検索する."
  (interactive (list (read-string "Grep (buffer): ") current-prefix-arg))
  (if arg
      (occur regexp)                             ; 現在のバッファ
      (multi-occur (valid-buffer-list) regexp))) ; 全てのバッファ 

;;; @@Xyzzy-only
(defvar *kill-buffer-kills-scratch* nil
  "non-nilならば、kill-bufferで*scratch*が削除可能.")

(defun kill-scratch-hook ()
  (cond (*kill-buffer-kills-scratch*
         t)
        ((and (eq major-mode 'lisp-interaction-mode)
              (equal (buffer-name (current-buffer)) "*scratch*"))
         (erase-buffer)
         (when (and
                ;; (null inhibit-startup-message)
                initial-scratch-message)
           (insert initial-scratch-message)
           (set-buffer-modified-p nil))
         nil)
        (t t)))
(add-hook 'kill-buffer-query-functions 'kill-scratch-hook)

(defun* string-downcase (string &key (start 0) end)
  (concat (substring string 0 start)
          (downcase (substring string start end))
          (if end (substring string end) "")))

;; 正規表現のコンパイルってあったっけ？
(defun compile-regexp (regexp &optional case-fold)
  (warn "Undefined `compile-regexp'")
  regexp)

;; FIXME: ポイント位置でなくマウスの位置に表示される
;; left,topはピクセル指定？
;; (momentary-string-display STRING POINT)
(defun popup-string (string point &optional timeout)
  (require 'tooltip)
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
;; (autoload 'cl-prettyexpand "cl-extra")
;; (autoload 'cl-prettyprint "cl-extra")
;; (autoload 'cl-macroexpand-all "cl-extra")
(eval-when-compile (load "cl-extra"))

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
        ;; FIXME: なぜ色付けが効かない?
        (temp-buffer-show-hook '(font-lock-fontify-buffer))
        ;; (temp-buffer-show-hook  (copy-sequence temp-buffer-show-hook))
        )
    ;; (add-hook 'temp-buffer-show-hook 'font-lock-fontify-buffer)
    ;; (add-hook 'temp-buffer-show-hook
    ;;           (lambda ()
    ;;             (emacs-lisp-mode)
    ;;             ;; (font-lock-fontify-buffer)
    ;;             ))
    (unless form
      (return-from elisp-macroexpand-1))
    (with-output-to-temp-buffer #1=" *ELISP macroexpantion*"
      (with-temp-buffer
        (if *elisp-macroexpand-require-cl-function*
            (cl-prettyexpand form repeatedly)
            (cl-prettyprint (funcall (if repeatedly
                                         #'cl-macroexpand-all
                                         #'macroexpand)
                                     form)))
        (copy-to-buffer #1# (point-min) (point-max))))
    ))

(defun elisp-macroexpand-all ()
  (interactive)
  (elisp-macroexpand-1 t))

;; (fset 'lisp-indent-hook #'lisp-indent-function)


;; Local Variables:
;; mode: emacs-lisp
;; coding: utf-8
;; End:

;;; xyzzy.el ends here
