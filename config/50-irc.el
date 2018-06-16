;;; config/IRC

;; https://www.emacswiki.org/emacs/InternetRelayChat
;; https://www.emacswiki.org/emacs/rcirc
;; https://wiki.mozilla.org/IRC

;; M-x irc (or rcirc)
(with-eval-after-load 'rcirc
  (rcirc-track-minor-mode)
  (custom-set-variables
   '(rcirc-server-alist
     '(("irc.freenode.net" :channels ("#emacs"))
       ("irc.mozilla.org" :channels ("#rust" "#tokyo" "#japan"))))
   '(rcirc-log-flag t)
   '(rcirc-log-directory "~/.emacs.d/var/rcirc-log")
   )
  )
