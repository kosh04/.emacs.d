;;; cmd.el --- command-line tool like functions

(defun wc ()
  "Print newline, word, and byte counts for current buffer or region."
  (interactive)
  (let ((prefix-arg (if (region-active-p) nil ?\C-u)))
    (command-execute 'count-words-region)))

(defun tac ()
  "Concatenate and print buffers in reverse."
  (interactive "*")
  (if (region-active-p)
      (call-interactively #'reverse-region)
    (reverse-region (point-min) (point-max))))

(defalias 'expand #'untabify)
(defalias 'unexpand #'tabify)

;; keymap
(global-set-key (kbd "M-=") 'wc)
