;;; config/View

;; デフォルトは読み取り専用で開く
(setq view-read-only t)

(with-eval-after-load 'view
  (let ((map view-mode-map))
    ;; vi-like
    (define-key map "h" 'backward-char)
    (define-key map "l" 'forward-char)
    (define-key map "j" 'next-line)
    (define-key map "k" 'previous-line)
    ;; less-like
    (define-key map "N" 'View-search-last-regexp-backward)

    (define-key map (kbd "DEL") 'ignore)
    (define-key map (kbd "RET") 'ignore)
    ))