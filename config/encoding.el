;;; config/encoding.el

;; UTF-8 で統一したいが Windows の日本語ファイル名が文字化けする...
;;(prefer-coding-system 'utf-8-unix)
;;(prefer-coding-system 'cp932)

(defun detect-and-decode-string (string)
  (mapcar #'(lambda (encoding)
              (cons encoding (decode-coding-string string encoding)))
          (detect-coding-string string)))

(fset 'encode #'encode-coding-string)
(fset 'decode #'decode-coding-string)
(fset 'detect #'detect-coding-string)

;; config/encoding.el ends here.
