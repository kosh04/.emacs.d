;;; typespec.el --- extra type definitions

;; Copyright (C) 2016  KOBAYASHI Shigeru

;; Keywords: lisp, internal, maint

;; inspire to
;; - https://gitlab.common-lisp.net/alexandria/alexandria
;; - https://github.com/m2ym/trivial-types

;;; Code:

(eval-when-compile
  (require 'cl-lib))

;; lists

;; TODO: (defun proper-list-p () ...)
;; TODO: (defun circular-list-p () ...)

(cl-deftype proper-list ()
  "Type designator for proper list."
  `(and list (satisfies proper-list-p)))

(cl-deftype circular-list ()
  "Type designator for circular list."
  `(satisfies circular-list-p))

;; sequences

(cl-deftype proper-sequence ()
  "Type designator for proper sequences."
  `(or proper-list
       (and (not list) sequence)))

;; strings

(cl-deftype string-designator ()
  "A string designator type.
see URL `http://clhs.lisp.se/Body/26_glo_s.htm#string_designator'"
  `(or symbol string character))

;; types

(defvar typespec-array-dimension-limit #x10000 "?")

(cl-deftype array-index (&optional (length (1- typespec-array-dimension-limit)))
  `(integer 0 (,length)))

(cl-deftype array-length (&optional (length (1- typespec-array-dimension-limit)))
  `(integer 0 ,length))

(provide 'typespec)

;;; typespec.el ends here
