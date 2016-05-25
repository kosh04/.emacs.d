;;; unicode-escape.el -*- lexical-binding: t; -*-

;; Original: https://gist.github.com/kosh04/568800
;; Version: 1.0-beta
;; Package-Requires: ((emacs "24") (names "0.5"))
;; Keywords: i18n

;;; Commentary:

;; Escape and unescape unicode notations. "\uNNNN" <-> "\\uNNNN"

;;; Issue:

;; - `unicode-unescape' command is out of emacs-lisp namespace.
;;   Commands should provide only `unicode-escape' ?
;; - default value of `unicode-escape-enable-surrogate-pair'

;;; Code:

(eval-when-compile
  (require 'names)
  (require 'rx)
  (require 'cl-lib))

;;;###autoload
(define-namespace unicode-escape-

(defvar enable-surrogate-pair t
  "escape non-BMP characters as surrogate pair.")

(defvar -re-unicode
  (rx (not ascii))
  "Regex matches a non-ascii character.")

(defconst -re-escaped
  (rx (or (1+ "\\u" (= 4 xdigit))
          (1+ "\\U" (= 8 xdigit))))
  "Regex matches 1 (or more) unicode \\uNNNN or \\UNNNNNNNN notation.")

(defsubst -unicode-to-pair (char)
  "Translate code point CHAR to surrogate pair [high low]."
  (cl-check-type char (integer #x10000 #x10FFFF))
  (let ((code (- char #x10000)))
    (vector (logior #xD800 (ash code -10))
            (logior #xDC00 (logand code #x03FF)))))

(defsubst -pair-to-unicode (pair)
  "Translate surrogate pair PAIR to original code point."
  (let ((hi (aref pair 0))
        (lo (aref pair 1)))
    (cl-check-type hi (integer #xD800 #xDBFF))
    (cl-check-type lo (integer #xDC00 #xDFFF))
    (+ (ash (logand hi #x03FF) 10)
       (ash (logand lo #x03FF)  0)
       #x10000)))

(defun -escape (obj &optional surrogate-pair)
  "Escape object OBJ (character or string)."
  (cl-typecase obj
    (sequence
     (cl-labels ((escape (c) (-escape c surrogate-pair)))
       (apply #'concat (mapcar #'escape (vconcat obj)))))
    (character
     ;; (-escape ?\U00002603)   => "\\u2603"
     ;; (-escape ?\U0001F363)   => "\\uD83C\\uDF63"
     ;; (-escape ?\U0001F363 t) => "\\U0001F363"
     (let ((non-BMP (and (<= #x10000 obj) (<= obj #x10FFFF))))
       (cond ((and non-BMP surrogate-pair)
              (let ((pair (-unicode-to-pair obj)))
                (format "\\u%04X\\u%04X" (aref pair 0) (aref pair 1))))
             (non-BMP
              (format "\\U%08X" obj))
             (t
              (format "\\u%04X" obj)))))
    (otherwise
     (signal 'wrong-type-argument `(char-or-string-p ,obj)))))

(defun -parse-escaped-string (s)
  "Separate unicode notation string S to character set."
  ;; e.g. "\\uXXXX\\uYYYY\\uZZZZ" => (?\uXXXX ?\uYYYY ?\uZZZZ)
  (let ((slen (length s))
        (step (if (eql (aref s 1) ?U)
                  (length "\\UNNNNNNNN")
                  (length "\\uNNNN"))))
    (cl-loop for i from 0 by step
             while (< i slen)
             collect (let ((hex (substring s (+ i 2) (+ i step))))
                       (string-to-number hex 16)))))

(defun -unescape (notations &optional surrogate-pair)
  "Unescape unicode notations."
  ;; e.g. "\\u0061\\uD83C\\uDF63\\u00A0" => "a\U0001F363\u00A0" (surrogate-pair=t)
  (concat (cl-reduce
           #'(lambda (acc char)
               (let ((hi (car (last acc)))
                     (lo char))
                 (cond ((and hi surrogate-pair
                             (<= #xD800 hi) (<= hi #xDBFF)
                             (<= #xDC00 lo) (<= lo #xDFFF))
                        (setf (car (last acc)) (-pair-to-unicode (vector hi lo)))
                        acc)
                       (t `(,@acc ,char)))))
           (-parse-escaped-string notations)
           :initial-value nil)))

(cl-defun -escape-string (string &key unescape (surrogate-pair unicode-escape-enable-surrogate-pair))
  "Escape STRING to unicode notation (\\uNNNN).
If SURROGATE-PAIR is non-nil, non-BMP characters (U+0000..U+10FFFF)
convet a 2-byte seqeunce such as surrogate pair."
  (let ((regexp  (if unescape -re-escaped -re-unicode))
        (replace (if unescape
                     #'(lambda (s) (-unescape s surrogate-pair))
                     #'(lambda (s) (-escape   s surrogate-pair))))
        (case-fold-search nil))
    (replace-regexp-in-string regexp replace string t t)))

(cl-defun -unescape-string (string &key (surrogate-pair unicode-escape-enable-surrogate-pair))
  "Unescape unicode string in STR.
If SURROGATE-PAIR is non-nil, Surrogate pairs will be converted to
original code point."
  (-escape-string string :unescape t :surrogate-pair surrogate-pair))

(defun -escape-region (start end &optional unescape)
  "Escape unicode characters from region START to END.
If argument UNESCAPE is non-nil, switch to unescape converter."
  (interactive "*r\nP")
  (let ((regexp  (if unescape -re-escaped -re-unicode))
        (replace (if unescape #'-unescape #'-escape))
        (surrogate-pair unicode-escape-enable-surrogate-pair)
        (case-fold-search nil))
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (goto-char (point-min))
        (while (re-search-forward regexp nil t)
          (let ((newtext (funcall replace (match-string 0) surrogate-pair)))
            (replace-match newtext t t)))))))

(defun -unescape-region (start end)
  "Unescape unicode notations from region START to END."
  (interactive "*r")
  (-escape-region start end t))

;; export
:autoload (defalias 'unicode-escape        #'-escape-string)
:autoload (defalias 'unicode-escape-region #'-escape-region)
:autoload (defalias 'unicode-unescape        #'-unescape-string)
:autoload (defalias 'unicode-unescape-region #'-unescape-region)
)

(provide 'unicode-escape)
;;; unicode-escape.el ends here
