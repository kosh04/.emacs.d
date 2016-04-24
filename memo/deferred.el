;;; memo/deferred -*- lexical-binding: t -*-

(require 'deferred)
(load "~/Documents/GitHub/emacs-deferred/test-deferred.el")

;; テストはどうやって書く？
(progn
  (clear)
  (let (last-value)
    (nextc
     ($ (parallel
         (lambda () 0)
         (lambda () 1)))
     (setq last-value x))
    (flush)
    last-value))
;;=> (0 1)

(progn
  (setq deferred:queue nil)
  (lexical-let (value)
    (deferred:nextc
      (deferred:$
        (request-deferred "http://httpbin.org/get" :parser 'json-read)
        (deferred:nextc it
          (lambda (response)
            (message "recv: %S" (request-response-data response))
            (request-response-data response))))
      (lambda (x)
        (setq value x)))
    (deferred:flush-queue!)
    value))
;;=> nil ?
