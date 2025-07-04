;;; init.el --- .emacs

(defvar user-init-minimum-file
  (locate-user-emacs-file "init-minimum.el")
  "最小構成の設定ファイル")

(defvar user-init-private-file
  (locate-user-emacs-file "init-private.el")
  "マシン固有の設定ファイル")

(load user-init-minimum-file t)

;; Separate customization setting (do not overwrite `user-init-file')
(setq custom-file (locate-user-emacs-file "custom.el"))
;;(load custom-file t)

;; Load config/nn-xxx.el
(add-to-list 'load-path (locate-user-emacs-file "site-lisp/init-loader"))
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

;; Disable message: "For information about GNU Emacs and the GNU system, type \\[about-emacs]."
(setq inhibit-startup-echo-area-message "kosh")

(provide 'init-el)
;;; init.el ends here
