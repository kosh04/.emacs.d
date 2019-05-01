;;; config/Pocket Reader

(use-package pocket-reader
  :custom
  ;; Org 利用してない
  (pocket-reader-open-url-default-function #'eww)
  (pocket-reader-pop-to-url-default-function
   (lambda (url)
     (save-window-excursion (eww url))
     (pop-to-buffer "*eww*")))
  (pocket-reader-finalize-hook
   '(;;pocket-reader--apply-faces  ; 頻繁にバグるので色つけ要らない
     pocket-reader--add-spacers
     ))
)

;; (custom-set-variables
;;  '(pocket-lib-token-file (locate-user-emacs-file "var/pocket-lib-token.json"))
;;  )
