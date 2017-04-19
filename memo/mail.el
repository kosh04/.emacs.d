;;; memo/Mail

;; Link:
;; - https://www.emacswiki.org/emacs/SendingMail

(require 'smtpmail)
(custom-set-variables
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-mail-address "..."))

;; Mew
(with-eval-after-load "mew"
  (define-key mew-summary-mode-map "g" 'mew-status-update)
  )

;; Gmail を利用したい

;; http://cachestocaches.com/2017/3/complete-guide-email-emacs-using-mu-and-
