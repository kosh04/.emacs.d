;;; config/w32.el

(set-face-attribute 'default nil :family "Consolas" :height 110)
;; (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "MeiryoKe_Console"))
;; (setq face-font-rescale-alist '(("MeiryoKe_Console" . 1.08)))

;; NOTE: gnutls.c: [1] Note that the security level of the
;; Diffie-Hellman key exchange has been lowered to 256 bits and this
;; may allow decryption of the session data
(with-eval-after-load "gnutls"
  (setq gnutls-min-prime-bits 1024))

(add-to-list 'exec-path (getenv "SBCL_HOME"))
