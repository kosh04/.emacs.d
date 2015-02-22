;;; memo/buffers.el

(defun list-buffers-by-name ()
  "バッファ一覧の出力をソートする."
  (interactive)
  (let ((buffers (sort (buffer-list)
                       (lambda (x y)
                         (string< (buffer-name x) (buffer-name y))))))
    (mapc #'bury-buffer buffers)
    (list-buffers)
    (buffer-list)))

(defun buffer-invisible-p (&optional buffer)
  (integerp (string-match "^ " (buffer-name buffer))))

;; (remove-if-not #'buffer-invisible-p (buffer-list))

;; @@list-buffers
(list-buffers)
(ibuffer)
(call-interactively 'electric-buffer-list)
(iswitchb-buffer)
(call-interactively 'bs-show)
