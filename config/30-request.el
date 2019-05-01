;;; config/Request

(use-package request
  ;;:defer t
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

  ;; https://github.com/tkf/emacs-request/issues/111
  (when (and (eq system-type 'windows-nt)
             (file-exists-p "C:\\WINDOWS\\system32\\curl.exe"))
    (advice-add 'request--curl-command
                :filter-return
                (lambda (x)
                  "curl.exe since Windows 10 RS4 (1803) dosn't support --compressed option."
                  (delete "--compressed" x))))
  )
