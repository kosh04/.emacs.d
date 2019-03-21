;;; lingr-irc.el --- Lingr client using ERC

;;; Install

;; $ go get github.com/mattn/go-lingr/lingr-ircd
;; $ edit ~/.authinfo.gpg
;; > machine localhost login NICKNAME port PORT password PASSWORD
;; > machine localhost login lingr-ircd port PORT passowrd APIKEY
;; $ lingr-ircd -addr=:PORT -apikey=APIKEY

;;; Code:

(require 'subr-x)

(defvar lingr-irc-command "lingr-ircd")
(defvar lingr-irc-port 26667)

(defun lingr-irc-start-process ()
  (if-let* ((name lingr-irc-command)
	    (port lingr-irc-port)
	    (auth (car (auth-source-search :user name :port port)))
	    (pass (cl-getf auth :secret)))
      (make-process :name name
                    :buffer (get-buffer-create " *lingr-ircd*")
                    :command (list name (format "-addr=:%d" port)
				   (format "-apikey=%s" (funcall pass))))))

;;;###autoload
(defun lingr-irc ()
  "[user]Start Lingr-irc."
  (interactive)
  (unless (process-live-p (get-process lingr-irc-command))
    (lingr-irc-start-process))
  (if-let* ((port lingr-irc-port)
	    (auth (car (auth-source-search :host "localhost" :port port)))
            (nick (cl-getf auth :user))
            (pass (cl-getf auth :secret)))
      (erc :server "localhost"
           :port port
           :nick nick
           :password (funcall pass))))

(provide 'lingr-irc)
