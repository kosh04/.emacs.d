;;; site-lisp/unicode-escape.el

;; Original: https://gist.github.com/kosh04/568800
;; Package-Requires: ((emacs "24") (names "0.5"))

(require 'names)

(define-namespace unicode-escape:

(defconst -re-escaped "\\\\u\\([[:xdigit:]]\\{4\\}\\)")

(defsubst -escape (str)
  (format "\\u%04x" (string-to-char str)))

(defsubst -unescape (hexstr)
  (string (string-to-number hexstr 16)))

(defun escape (str)
  "文字列STRをUncodeエスケープする."
  (replace-regexp-in-string "." #'-escape str nil t))

(defun unescape (str)
  "Unicodeエスケープ文字列STRを復元する."
  (replace-regexp-in-string -re-escaped
                            (lambda (s)
                              (-unescape (match-string 1 s)))
                            str nil t))

(defun escape-region (start end)
  "指定した範囲の文字列をUnicodeエスケープする."
  (interactive "*r")
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    (while (re-search-forward "." nil t)
      (replace-match (-escape (match-string 0)) nil t))))

(defun unescape-region (start end)
  "指定した範囲のUnicodeエスケープ文字列を復元する."
  (interactive "*r")
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    (while (re-search-forward -re-escaped nil t)
      (replace-match (-unescape (match-string 1)) nil t))))

;; export
:autoload (defalias 'unicode-escape #'escape)
:autoload (defalias 'unicode-unescape #'unescape)
:autoload (defalias 'unicode-escape-region #'escape-region)
:autoload (defalias 'unicode-unescape-region #'unescape-region)
)

(provide 'unicode-escape)
;;; unicode-escape.el ends here
