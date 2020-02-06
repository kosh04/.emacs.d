;;; config/whitespace.el

(custom-set-variables
 '(whitespace-style '(tabs face trailing lines-tail newline-mark))
 ;; N+1行目以降をハイライト (lines-tail)
 '(whitespace-line-column fill-column)
 ;;'(show-trailing-whitespace t)
 )

(defun user/show-trailing-whitespace ()
  (setq show-trailing-whitespace t))

(with-eval-after-load 'whitespace
  (setf (cdr (assq 'newline-mark whitespace-display-mappings))
        '(?\n [?\u21B5 ?\n] [?$ ?\n]))       ;; ↵
  )

;;(add-hook 'prog-mode-hook 'whitespace-mode)
