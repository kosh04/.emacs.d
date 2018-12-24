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


(with-eval-after-load 'erc
  (setq erc-fill-column 100)
  (setq erc-kill-queries-on-quit t)
  (setq erc-kill-server-buffer-on-quit t)
  ;;(setq erc-join-buffer 'frame)
  ;; (setq erc-interpret-mirc-color t)
  ;; (setq erc-kill-server-buffer-on-quit t)
  (add-hook 'erc-mode-hook 'goto-address-mode)
  ;; (erc-truncate-mode +1)
  ;; (erc-spelling-mode +1)
  (erc-timestamp-mode +1)

  ;; FIXME: ログの設置場所が固定されない..
  (require 'erc-log)
  (custom-set-variables
   '(erc-log-channels-directory
     (let ((dir (locate-user-emacs-file "var/log/erc")))
       (unless (file-exists-p dir)
         (make-directory dir t))
       dir))
   '(erc-log-insert-log-on-open t))
  (erc-log-enable)
  )

