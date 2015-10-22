;;; config/windows-conf.el

;; Font
(set-face-attribute 'default nil :family "Consolas" :height 110)
;; (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "MeiryoKe_Console"))
;; (setq face-font-rescale-alist '(("MeiryoKe_Console" . 1.08)))

;; NOTE: gnutls.c: [1] Note that the security level of the
;; Diffie-Hellman key exchange has been lowered to 256 bits and this
;; may allow decryption of the session data
(with-eval-after-load "gnutls"
  (setq gnutls-min-prime-bits 1024))

;; NTEmacs64 24.5-IME-patched
;; https://github.com/chuntaro/NTEmacs64

(setq default-input-method "W32-IME")
(setq-default w32-ime-mode-line-state-indicator "[－]")
(setq w32-ime-mode-line-state-indicator-list '("[－]" "[あ]" "[－]"))
(w32-ime-initialize)
(add-hook 'w32-ime-on-hook #'(lambda () (set-cursor-color "brown")))
(add-hook 'w32-ime-off-hook #'(lambda () (set-cursor-color "black")))

;; VC-Git
;; 外部プロセスを抑制する
(with-eval-after-load 'vc-hooks
  (setq vc-handled-backends (delete 'Git vc-handled-backends))
  (remove-hook 'find-file-hook 'vc-find-file-hook)
  (remove-hook 'kill-buffer-hook 'vc-kill-buffer-hook))

;; プチフリーズはGCが原因かもしれない
;;(setq gc-cons-threshold 800000 gc-cons-percentage 0.1)
(setq gc-cons-threshold (* gc-cons-threshold 5))
(setq gc-cons-percentage 0.5)
