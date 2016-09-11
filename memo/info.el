;;; @@Info

(require 'info)

(defun info-elisp-manual ()
  "Display the Emacs Lisp manual in Info mode."
  (interactive)
  (info "(elisp)"))
(Info-goto-node "(elisp)")               ; info-mode でインタラクティブに呼び出してみる(g)
(call-interactively #'info-lookup-symbol)
(directory-files (car Info-directory-list) nil "elisp*")
(info "(elisp)Regular Expressions")
(info "(elisp)Mapping Functions")
(info "(elisp)Definition of signal")
(info "(elisp)Definition of mapatoms")
(info "(libc)Formatted Output Functions")

(defun info-lookup-topic ()
  (interactive)
  (info "(elisp)")
  (call-interactively 'Info-index))

;; texinfo から info ファイルを生成する
;; M-x texinfo-format-buffer               ; emacs から
;; $ emacs --batch -Q -l texinfmt --funcall batch-texinfo-format *.texinfo
