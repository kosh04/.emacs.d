;;; init.el --- .emacs

(defvar user-init-minimum-file
  (expand-file-name "init-minimum.el" (file-name-directory #$)))

(defvar user-init-private-file
  (locate-user-emacs-file "init-private.el")
  "マシン固有の設定ファイル")

(load user-init-minimum-file t)

;; Enable installed packages
;;(setq package-enable-at-startup t)
;; TODO: 現状 init-loader の為だけに呼び出しているのでパッケージをローカル化する？
(package-initialize)

;; Separate customization setting (do not overwrite `user-init-file')
(setq custom-file (locate-user-emacs-file "custom.el"))
;;(load custom-file t)

;; Load config/nn-xxx.el
(require 'init-loader)
(setq init-loader-directory (locate-user-emacs-file "config"))
(setq init-loader-show-log-after-init 'error-only)
(init-loader-load)

;; TODO: 環境別ファイルの命名規則をGo言語風にする (prefixでなくsuffixで表現したい)
;; (custom-set-variables
;;  '(init-loader-windows-regexp "-windows\\'")
;;  '(init-loader-linux-regexp )
;;  '(init-loader-cocoa-emacs-regexp)
;;  '(init-loader-nw-regexp )
;;  )

(load user-init-private-file t)

(provide 'init-el)
;;; init.el ends here
