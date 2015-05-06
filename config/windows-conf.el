;;; config/windows-conf.el

(set-face-attribute 'default nil :family "Consolas" :height 110)
;; (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "MeiryoKe_Console"))
;; (setq face-font-rescale-alist '(("MeiryoKe_Console" . 1.08)))

;; NOTE: gnutls.c: [1] Note that the security level of the
;; Diffie-Hellman key exchange has been lowered to 256 bits and this
;; may allow decryption of the session data
(with-eval-after-load "gnutls"
  (setq gnutls-min-prime-bits 1024))

(add-to-list 'exec-path (getenv "SBCL_HOME"))

;; Ispell
(setq ispell-program-name "/cygwin/bin/aspell")

;; NTEmacs64 24.5-IME-patched
;; https://github.com/chuntaro/NTEmacs64

(setq default-input-method "W32-IME")
;; FIXME: モードラインの全角半角フォントの高さを統一してから適用する
;;(setq-default w32-ime-mode-line-state-indicator "[--]")
;;(setq w32-ime-mode-line-state-indicator-list '("[--]" "[あ]" "[--]"))
(w32-ime-initialize)
(add-hook 'w32-ime-on-hook #'(lambda () (set-cursor-color "brown")))
(add-hook 'w32-ime-off-hook #'(lambda () (set-cursor-color "black")))
