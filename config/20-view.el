;;; config/View

;; デフォルトは読み取り専用で開く
(setq view-read-only nil)

(with-eval-after-load 'view
  (let ((map view-mode-map))
    ;; vi-like
    (define-key map "h" 'backward-char)
    (define-key map "l" 'forward-char)
    (define-key map "j" 'next-line)
    (define-key map "k" 'previous-line)

    (define-key map (kbd "DEL") 'ignore)
    (define-key map (kbd "RET") 'ignore)
    ))
