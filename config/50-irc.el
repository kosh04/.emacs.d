;;; config/IRC

;; https://www.emacswiki.org/emacs/EmacsChannel
;; https://www.emacswiki.org/emacs/InternetRelayChat
;; https://www.emacswiki.org/emacs/rcirc

;; https://wiki.debian.org/IRC
;; https://wiki.mozilla.org/IRC

(setf (getenv "IRCNICK") "kosh04")

(defun user/irc ()
  (interactive)
  (erc :server "irc.sdf.org")
  (erc-tls :server "irc.rekt.network" :port 6697)
  ;; https://unix.chat/
  (erc-tls :server "unix.chat" :port 6697)
  (erc-tls :server "irc.mozilla.org" :port 6697)
  )

(use-package erc
  :custom
  (erc-kill-queries-on-quit t)
  (erc-kill-server-buffer-on-quit t)
  ;; erc-fill
  (erc-fill-column 100)
  ;;(erc-join-buffer 'frame)
  ;;(erc-interpret-mirc-color t)
  ;; FIXME: ログの設置場所が固定されない..
  (erc-log-channels-directory
   (let ((dir (locate-user-emacs-file "cache/erc-log")))
     (unless (file-exists-p dir)
       (make-directory dir t))
     dir))
  (erc-log-insert-log-on-open t)

  :config
  (setq erc-default-server "irc.libera.chat")
  (add-hook 'erc-mode-hook 'goto-address-mode)
  ;; (erc-truncate-mode +1)
  ;; (erc-spelling-mode +1)
  (erc-timestamp-mode +1)
  (erc-log-mode +1)
  )

;; (use-package* erc-log
;;   :demand 
;;   :custom
;;   :config
;;   )
