;;; config/cocoa-emacs-conf.el

(add-to-list 'exec-path "/usr/local/bin")

;; Font
(set-face-attribute 'default nil :family "Menlo" :height 150)
;;(set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Hiragino Kaku Gothic ProN"))

;;(add-to-list 'default-frame-alist '(alpha . (0.75 0.75)))
(set-frame-parameter nil 'alpha 90)

