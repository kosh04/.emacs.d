;;; config/Request

(use-package request
  :commands (request)
  :config
  (defun toggle-request-logger ()
    "[user] Toggle `request' message level."
    (interactive)
    (if (eq request-log-level -1)
        (setq request-log-level     'debug
              request-message-level 'debug)
        (setq request-log-level     -1
              request-message-level 'warn))
    (message "log=%s,msg=%s" request-log-level request-message-level))
  )
