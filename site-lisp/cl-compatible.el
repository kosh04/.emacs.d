;;; cl-compatible.el --- Common Lisp flavor functions/variables

;; Author: KOBAYASHI Shigeru (kosh) <shigeru.kb@gmail.com>
;; Version: 0.1
;; Created: 2015-02-23
;; Keywords: lisp,common-lisp

(eval-when-compile
  (require 'cl))
(require 'cl-lib)

;; @@ Data-Type

(fset 'compiled-function-p #'byte-code-function-p)

;; @@ Documentation

(defun cl-documentaiotn (x doc-type)
  "Get documentation string assosiated with X depends on DOC-TYPE."
  (cl-case doc-type
    (function (documentation x))
    (variable (or (documentation-property x 'variable-documentation)
                  (documentation-property (ignore-errors
                                            (indirect-variable x))
                                          'variable-documentation)))
    ;; ...
    ))

;; @@ Variable

(defvaralias '*modules* 'features)
(defvaralias '*load-pathname* 'load-file-name)
(defvaralias '*load-paths* 'load-path
  "The list of directories where programs are searched on LOAD etc.")

(fset 'defconstant #'defconst)

(defmacro defparameter (symbol value &optional docstring)
  `(if (boundp ',symbol)
       (progn
         (set ',symbol ,value)
         ,(if docstring
              `(put ',symbol 'variable-documentation ,docstring))
         ',symbol)
       (defvar ,symbol ,value ,docstring)))

;; 代入する変数が束縛されているかどうかは実行時まで分からない
;; (defmacro defparameter (symbol value &optional doc-string)
;;   (if (boundp symbol)
;;       `(progn
;;          (set ',symbol ,value)
;;          ,(if doc-string
;;               `(put ',symbol 'variable-documentation ,doc-string))
;;          ',symbol)
;;       `(defvar ,symbol ,value ,doc-string)))

;; cl-macs.el:1716
;; Some more Emacs-related place types.
;; (defsetf ...)

;; @@ 制御構造

;; Common Lisp `tagbody' for Emacs Lisp
;; http://www.emacswiki.org/emacs/tagbody.el

;; @@ Function

;; @@ Macro

(defun macro-function (symbol &optional environment)
  (declare (ignore environment))
  (and (fboundp symbol)
       (let ((fn (symbol-function symbol)))
         (and (eq (car-safe fn) 'macro)
              fn))))

;; @@ Symbol

(defun find-symbol (string)
  (intern-soft string))

(cl-defmacro do-all-symbols ((var &optional result-form) &body body)
  "Iterates on every symbols."
  (declare (indent 1))
  `(progn
     (mapatoms #'(lambda (,var) ,@body))
     ,result-form))

;; symbol-function ~= indirect-function

;; @@ Number (Math)
;; Bignumber を扱えない点に注意

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

;; @@ Character

(fset 'char #'elt)

(defun alpha-char-p (char)
  (integerp (string-match "[A-Za-z]" (char-to-string char))))

(defun alphanumericp (char)
  (integerp (string-match "[A-Za-z0-9]" (char-to-string char))))

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

(defun char-downcase (char)
  (check-type char character)
  (downcase char))

(defun char-upcase (char)
  (check-type char character)
  (upcase char))

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

;; (fset 'char-int #'identity)

;; `describe-char-unicodedata-file' を設定していれば利用可能
;; Help > describe-variable > describe-char-unicodedata-file
(autoload 'describe-char-unicode-data "descr-text"
  "Return a list of Unicode data for unicode CHAR.")

(defun char-name (character)
  (nth 1 (assoc "Name" (describe-char-unicode-data character))))

;; (defun name-char (name) ?)

(defun char= (char &rest more-chars)
  (let ((case-fold-search nil))
    ;; (every (lambda (c) (char-equal c char)) more-chars)
    (do* ((chars more-chars (cdr chars))
          (c #1=(car chars) #1#))
         ((null chars) t)
      (if (not (char-equal c char))
          (return nil)))
    ))

;; @@ Sequence

(defun* string-downcase (string &key (start 0) end)
  (concat (substring string 0 start)
          (downcase (substring string start end))
          (if end (substring string end) "")))

;; NOTE: Emacs-Lisp において string= は string-equal のエイリアス.
;; おまけに大文字小文字を区別しない

(when (and (not (featurep 'cl-compatible))
           (fboundp 'cl-string=))
  (warn "cl-string= is already defined."))

(cl-defun cl-string= (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "大文字小文字を区別して文字列を比較する."
  (eq (compare-strings string1 start1 end1
                       string2 start2 end2
                       nil)
      t))

(cl-defun cl-string-equal (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "大文字小文字を区別せずに文字列を比較する."
  (eq (compare-strings string1 start1 end1
                       string2 start2 end2
                       t)
      t))

;; (cl-defun cl-string-equal (string1 string2 &key (start1 0) end1 (start2 0) end2)
;;   (cl-check-type string1 string)
;;   (cl-check-type string2 string)
;;   (cl-equalp (substring string1 start1 end1)
;;              (substring string2 start2 end2)))

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

(defun* parse-integer (string &key start end radix junk-allowed)
  (declare (ignore junk-allowed))
  (string-to-number (substring string (or start 0) end) radix))

;; @@ Input/Output
(defun princ-to-string (object &optional stream)
  (with-output-to-string (princ object stream)))

;; @@ Filename

(fset 'merge-pathnames #'expand-file-name)
(fset 'file-namestring #'file-name-nondirectory)
(fset 'directory-namestring #'file-name-directory)
(fset 'pathname-type #'file-name-extension)
;;(fset 'file-info-attributes #'?)

(defun user-homedir-pathname ()
  (expand-file-name "~/"))

;; CLにファイルのリンク先を参照する関数はあるのか？
;; file-truename, file-chase-links
(defun* directory (pathname &key absolute recursive wild depth file-only
                            show-dots count directory-only callback file-info)
  (declare (ignore recursive depth show-dots count))
  (cl-labels ((filter (f seq)
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

;; (byte-compile-dest-file "foo.el")    => "xyzzy.elc"
;; (byte-compile-dest-file "foo.el.gz") => "xyzzy.elc"
(defun compile-file-pathname (pathname)
  (require 'bytecomp)
  (byte-compile-dest-file pathname))

(defun file-length (pathname)
  "Return PATHNAME size in bytes."
  (nth 7 (file-attributes pathname)))

;; @@ Date-Time

;; Common Lisp (xyzzy) features `universal-time'
;; Emacs Lisp          features `UNIX-time'
;; (fset 'get-universal-time #'current-time)
;; (fset 'encode-universal-time #'encode-time)
;; (fset 'decode-universal-time #'decode-time)

(fset 'get-internal-real-time #'get-internal-run-time) ; ?

(defun get-decoded-time ()
  (decode-time (current-time)))

;; @@ Condition

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

;; @@ Debug

(autoload 'trace-is-traced "trace")
(autoload 'untrace-all "trace")
(autoload 'untrace-function "trace")

(defmacro trace (&rest function-name)
  (if function-name
      `(mapc #'trace-function ',function-name)
      `(let (fns)
         (mapatoms (lambda (s)
                     (if (trace-is-traced s)
                         (push s fns))))
         (nreverse fns))))

(defmacro untrace (&rest function-name)
  (if function-name
      `(mapc #'untrace-function ',function-name)
      `(untrace-all)))

;; @@ Misc

(fset 'gc #'garbage-collect)

(provide 'cl-compatible)

;;; cl-compatible.el ends here
