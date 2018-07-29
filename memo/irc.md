# IRC

## Client Packages

### rcirc, simple IRC client.

```emacs-lisp
;; M-x irc (or rcirc, erc)
(with-eval-after-load 'rcirc
  (rcirc-track-minor-mode)
  (custom-set-variables
   '(rcirc-server-alist
     '(("irc.freenode.net" :channels ("#emacs"))
       ("irc.mozilla.org" :channels ("#rust" "#tokyo" "#japan"))))
   '(rcirc-log-flag t)
   '(rcirc-log-directory (locate-user-emacs-file "var/log/rcirc"))
   )
  )
```

### erc

### Circe

https://github.com/jorgenschaefer/circe
