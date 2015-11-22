;;; config/cocoa-emacs-conf.el

;; [alt] <-> [command]
(custom-set-variables
 '(mac-option-modifier  'alt)
 '(mac-command-modifier 'meta))

;; Path
;;(add-to-list 'exec-path "/usr/local/bin")
;;(add-to-list 'exec-path "~/bin")


;; Font
(set-face-attribute 'default nil :family "Menlo" :height 150)
;;(set-fontset-font nil 'unicode (font-spec :family "Menlo"))
;;(set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Hiragino Kaku Gothic ProN"))

;; FIXME
(setq ispell-program-name "aspell")
