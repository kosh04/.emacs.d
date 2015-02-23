;;; memo/mail.el

(setq smtpmail-smtp-server "smtp.gmail.com")
(setq smtpmail-mail-address "...")

(eval-after-load "mew"
  (quote
   (progn
     (define-key mew-summary-mode-map "g" 'mew-status-update)
     )))
