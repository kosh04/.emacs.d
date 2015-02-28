;;; config/sgml.el

;; xml/xhtml/shml/html5?

(autoload 'sgml-quote "sgml-mode" nil t)
(fset 'sgml-quote-region #'sgml-quote)
(fset 'html-quote-region #'sgml-quote)

(use-package "sgml-mode"
  :bind (("C-," . sgml-tag)
         ("C-." . sgml-close-tag)))

(defun unhtml-region (start end)
  "HTMLタグを除去する."
  (interactive "*r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (while (re-search-forward "<[^>]*>" nil t)
        (replace-match "")))))

;;(define-key html-mode-map (kbd "C-c C-q") 'sgml-quote)
