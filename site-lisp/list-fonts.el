;;; list-fonts.el       -*- lexical-binding: t -*-

(define-derived-mode font-list-mode tabulated-list-mode "Font lists"
  "DOCSTRING"
  (setq tabulated-list-format
        [("*" 1)
         ("maker" 8)
         ("family" 30 t)
         ("weight" 12)
         ("slant" 8)
         ("widthtype" 10)
         ("adstyle" 8)
         ("pixelsz" 7)
         ("height" 6)
         ("horiz" 5)
         ("vert" 5)
         ("spacing" 7)
         ("width" 5)
         ("registry" 12)
         ("encoding" 8)])
  (setq tabulated-list-entries
        (mapcar (lambda (font)
                  ;; FIXME: How parse font name with hyphen. like "all-the-icon"
                  (list font (apply #'vector (split-string font "-"))))
                (x-list-fonts "*")))
  (setq eldoc-documentation-function #'tabulated-list-get-id))

(defun list-fonts* ()
  (interactive)
  (with-current-buffer (get-buffer-create "*Fonts*")
    (font-list-mode)
    (tabulated-list-init-header)
    (tabulated-list-print)
    (switch-to-buffer (current-buffer))))
