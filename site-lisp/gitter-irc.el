;;; gitter-irc.el

;; https://irc.gitter.im/

;; Setup:
;; write to ~/.netrc (or ~/.authinfo.gpg):
;; machine irc.gitter.im
;;   login NICKNAME
;;   password PASSWORD

;; shell alias gitter="emacs -q -nw -l ~/.emacs.d/site-lisp/gitter-irc.el -f gitter-irc"

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
         (auth (or (nth 0 (auth-source-search :host host))
                   (error "%s: authinfo not found" host)))
         (nick (plist-get auth :user))
         (pass (funcall (plist-get auth :secret))))
    (unless (and nick pass)
      (error "cannot found Nick/Pass in %s" host))
    (erc-tls :server host
             :nick nick
             :password pass)))

(provide 'gitter-irc)
