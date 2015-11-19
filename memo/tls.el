;;; memo/tls.el --- SSL/TLS 関係

;;; Link:

;; https://www.gnu.org/software/emacs/manual/html_mono/emacs-gnutls.html

;; how to enable GnuTLS for Emacs 24 on Windows
;; http://xn--9dbdkw.se/diary/how_to_enable_GnuTLS_for_Emacs_24_on_Windows/

;; NTEmacs では HTTPS 通信を行うために bin/libgnutls-28.dll が同梱している
;; ただし、証明書ファイルが見つからないため機能しているのか怪しい (*.pm, *.crt)

(require 'tls)
(require 'gnutls)

(setq gnutls-log-level 2)
(setq gnutls-verify-error t)

(add-to-list 'gnutls-trustfiles (expand-file-name "~/.emacs.d/etc/cacert.pem"))
(add-to-list 'gnutls-trustfiles (expand-file-name "/cygwin/usr/ssl/cert.pem"))

(add-to-list 'gnutls-trustfiles "/msys64/usr/ssl/cert.pem")
(add-to-list 'gnutls-trustfiles "/msys64/usr/ssl/certs/ca-bundle.crt")

;; ## 通常時のネットワーク状態での M-x `list-package' ログ
;;
;; Importing package-keyring.gpg...done
;; Contacting host: elpa.gnu.org:443
;; gnutls.c: [1] (Emacs) allocating credentials
;; gnutls.c: [2] (Emacs) allocating x509 credentials
;; gnutls.c: [2] (Emacs) using default verification flags
;; gnutls.c: [1] (Emacs) gnutls callbacks
;; gnutls.c: [1] (Emacs) gnutls_init
;; gnutls.c: [1] (Emacs) got non-default priority string: NORMAL
;; gnutls.c: [1] (Emacs) setting the priority string
;; gnutls.c: [1] (Emacs) non-fatal error: Resource temporarily unavailable, try again. [2793 times]
;; elpa.gnu.org certificate could not be verified.
;; gnutls.c: [1] (Emacs) certificate signer was not found: elpa.gnu.org
;; Failed to download `gnu' archive.
;; Contacting host: melpa.org:443
;; gnutls.c: [1] (Emacs) non-fatal error: Resource temporarily unavailable, try again. [81 times]
;; Contacting host: melpa.org:443
;; Contacting host: stable.melpa.org:443
;; gnutls.c: [1] (Emacs) non-fatal error: Resource temporarily unavailable, try again. [63 times]
;; Contacting host: stable.melpa.org:443
;; 22 packages can be upgraded; type `U' to mark them for upgrading.

;; ## plala
;;
;; Importing package-keyring.gpg...done
;; Contacting host: elpa.gnu.org:80 [2 times]
;; Contacting host: melpa.org:443
;; gnutls.c: [0] (Emacs) fatal error: The TLS connection was non-properly terminated.
;; Failed to download `melpa' archive.
;; Contacting host: stable.melpa.org:443
;; gnutls.c: [0] (Emacs) fatal error: The TLS connection was non-properly terminated.
;; Failed to download `melpa-stable' archive.
;; 1 package can be upgraded; type `U' to mark it for upgrading.
;; 1 package marked for upgrading.
