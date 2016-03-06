;;; config/whitespace.el

(custom-set-variables
 `(whitespace-style '(tabs face trailing lines-tail))
 ;; N+1行目以降をハイライト (lines-tail)
 `(whitespace-line-column ,fill-column))

;;(add-hook 'prog-mode-hook 'whitespace-mode)
