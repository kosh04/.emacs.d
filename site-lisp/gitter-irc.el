;;; gitter-irc.el

;; https://irc.gitter.im/

;; Setup:
;; write to .netrc:
;; machine irc.gitter.im
;;   login NICKNAME
;;   password PASSWORD

(require 'erc)
(require 'auth-source)
(require 'emojify nil t)

(with-eval-after-load 'erc
  (when (featurep 'emojify)
    (add-hook 'erc-mode-hook #'emojify-mode)))

;;;###autoload
(defun gitter-irc ()
  (interactive)
  (let* ((host "irc.gitter.im")
         (auth (car (auth-source-search :host host)))
         (nick (plist-get auth :user))
         (pass (funcall (plist-get auth :secret))))
    (unless (and nick pass)
      (error "cannot found Nick/Pass in %s" host))
    (erc-tls :server host
             :nick nick
             :password pass)))

(provide 'gitter-irc)
