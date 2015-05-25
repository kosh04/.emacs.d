;;; config/cocoa-emacs-conf.el

;; Path
;;(add-to-list 'exec-path "/usr/local/bin")
;;(add-to-list 'exec-path "~/bin")

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config (exec-path-from-shell-initialize))

;; Font
(set-face-attribute 'default nil :family "Menlo" :height 150)
;;(set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Hiragino Kaku Gothic ProN"))

;; Frame
;;(add-to-list 'default-frame-alist '(alpha . (0.75 0.75)))
(set-frame-parameter nil 'alpha 90)
