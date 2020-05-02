;;; config/View

;; ファイルは基本的に read-only+view-mode で開きたい

(setq view-read-only t)

(with-eval-after-load 'view
  (setq-default view-exit-action #'kill-buffer)
  (let ((map view-mode-map))
    ;; vi-like
    (define-key map "h" 'backward-char)
    (define-key map "l" 'forward-char)
    (define-key map "j" 'next-line)
    (define-key map "k" 'previous-line)
    ;; less-like
    (define-key map "N" 'View-search-last-regexp-backward)
    (define-key map "i" 'read-only-mode)

    ;;(define-key map (kbd "DEL") 'ignore)
    (define-key map (kbd "RET") 'ignore)
    ))

(add-hook 'find-function-after-hook 'view-mode)
