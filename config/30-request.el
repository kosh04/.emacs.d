;;; config/request.el

(with-eval-after-load 'request

(defun request--curl-must-raw-test (f &rest args)
  "[user] monkey patch for request.el with cURL.
see also `https://github.com/tkf/emacs-request/pull/27'."
  (let ((default-process-coding-system '(raw-text-unix . raw-text-unix)))
    (apply f args)))

(advice-add 'request--curl :around 'request--curl-must-raw-test)
;;(advice-remove 'request--curl 'request--curl-must-raw-test)

) ;; end eval-after-load
