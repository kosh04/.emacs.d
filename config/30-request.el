;;; config/Request

(use-package request
  :defer t
  :config
  (defun toggle-request-logger ()
    "[user] Toggle `request' message level."
    (interactive)
    (if (eq request-log-level -1)
        (setq request-log-level     'debug
              request-message-level 'debug)
        (setq request-log-level     -1
              request-message-level 'warn))
    (message "%s" (list request-log-level request-message-level))))