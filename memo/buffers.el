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
(electric-buffer-list nil)
(iswitchb-buffer)
(call-interactively 'bs-show)

(require 'uniquify)
;; forward 形式は見易いが、ファイル名が先頭でなくなるためバッファ切り替え操作に向いてない
(setq uniquify-buffer-name-style 'forward)
