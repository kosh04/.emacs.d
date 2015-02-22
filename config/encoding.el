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

;; なぜか utf-8 になっていたので
;; (set-language-environment "japanese") のせいかな?
'(when (eq system-type 'windows-nt)
  (setq default-process-coding-system
        ;; '(japanese-iso-8bit . japanese-iso-8bit)
        '(sjis-unix . sjis-unix)
        ))

;; config/encoding.el ends here.
