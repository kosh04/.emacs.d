;;; surrogate-pair.el --- Calculate unicode surrogate pair  -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Kobayashi Shigeru (kosh)
;; Keywords: i18n

;;; Commentary:

;; http://www.unicode.org/glossary/#surrogate_code_point

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(defun encode-utf-16-pair (obj)
  (unless (char-or-string-p obj)
    (signal 'wrong-type-argument `(char-or-string-p ,obj)))
  (if (stringp obj)
      (apply #'concat (mapcar #'encode-utf-16-pair (vconcat obj)))
    (let ((char obj))
      (cl-assert (and (<= #x10000 char) (<= char #x10FFFF)))
      (let ((code (- char #x10000)))
        (vector (logior #xD800 (ash code -10))
                (logior #xDC00 (logand code #x03FF)))))))

(defun decode-utf-16-pair (pair)
  (let ((hi (aref pair 0))
        (lo (aref pair 1)))
    (cl-assert (and (<= #xD800 hi) (<= hi #xDBFF)))
    (cl-assert (and (<= #xDC00 lo) (<= lo #xDFFF)))
    (+ (ash (logand hi #x03FF) 10)
       (logand lo #x03FF)
       #x10000)))

(defvar surrogate-pair--re-range "[\U00010000-\U0010FFFF]")
(defvar surrogate-pair--re-pairs "[\uD800-\uDBFF][\uDC00-\uDFFF]")

(defun surrogate-pair-encode (string)
  (replace-regexp-in-string surrogate-pair--re-range
                            #'encode-utf-16-pair
                            string nil t))

(defun surrogate-pair-decode (string)
  (replace-regexp-in-string surrogate-pair--re-pairs
                            (lambda (s)
                              (string (decode-utf-16-pair s)))
                            string nil t))

(provide 'surrogate-pair)
;;; surrogate-pair.el ends here
