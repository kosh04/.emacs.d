;;; config/Pocket-Reader

(use-package pocket-reader
  ;; [2019-05-18] コードを簡略化しようとしてforkしたはいいが割に合わなさそうだったので諦めた
  ;; 動かなくなったら元パッケージを利用するように
  :load-path "~/.emacs.d/site-lisp/pocket-reader"
  :custom
  ;; Org 利用してない
  (pocket-reader-open-url-default-function #'eww)
  (pocket-reader-pop-to-url-default-function
   (lambda (url)
     (save-window-excursion (eww url))
     (pop-to-buffer "*eww*")))
  (pocket-reader-finalize-hook
   '(;;pocket-reader--apply-faces  ; 頻繁にバグるので色つけ要らない
     ;;pocket-reader--add-spacers
     ))
  (pocket-reader-color-title nil)
  (pocket-reader-archive-on-open nil)
  :bind
  (:map pocket-reader-mode-map
        ("TAB" . pocket-reader-excerpt)
        ("g" . pocket-reader-refresh)
        ("w" . pocket-reader-copy-url)
        ("&" . pocket-reader-open-in-external-browser)
        )
  )

;; (custom-set-variables
;;  '(pocket-lib-token-file (locate-user-emacs-file "var/pocket-lib-token.json"))
;;  )

(use-package getpocket
  :pin #:manual
  :load-path "~/.emacs.d/site-lisp/getpocket.el/"
  :commands (getpocket)
  :config
  (getpocket-debug-mode +1))
