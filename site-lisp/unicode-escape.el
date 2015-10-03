;;; site-lisp/unicode-escape.el

;; Original: https://gist.github.com/kosh04/568800
;; Package-Requires: ((emacs "24") (names "0.5"))

(eval-when-compile
  (require 'names))

(define-namespace unicode-escape-

(defconst -re-escaped "\\\\u\\([[:xdigit:]]\\{4\\}\\)")
(defconst -re-unicode "[^[:ascii:]]")

(defsubst -escape (str)
  (format "\\u%04x" (string-to-char str)))

(defsubst -unescape (hexstr)
  (string (string-to-number hexstr 16)))

(defsubst -escape-string (str)
  "Escape unicode characters STR."
  (replace-regexp-in-string -re-unicode
                            (lambda (s)
                              (-escape (match-string 0 s)))
                            str nil t))

(defsubst -unescape-string (str)
  "Unescape unicode charaters STR."
  (replace-regexp-in-string -re-escaped
                            (lambda (s)
                              (-unescape (match-string 1 s)))
                            str nil t))

(defsubst -escape-region (start end)
  "Escape unicode characters from region START to END."
  (interactive "*r")
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    (while (re-search-forward -re-unicode nil t)
      (replace-match (-escape (match-string 0)) nil t))))

(defsubst -unescape-region (start end)
  "Unescape unicode characters from region START to END."
  (interactive "*r")
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    (while (re-search-forward -re-escaped nil t)
      (replace-match (-unescape (match-string 1)) nil t))))

;; export
:autoload (defalias 'unicode-escape #'-escape-string)
:autoload (defalias 'unicode-unescape #'-unescape-string)
:autoload (defalias 'unicode-escape-region #'-escape-region)
:autoload (defalias 'unicode-unescape-region #'-unescape-region)
)

(provide 'unicode-escape)
;;; unicode-escape.el ends here
