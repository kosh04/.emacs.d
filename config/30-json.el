;;; config/json.el

(require 'json)

(cl-defun json-decode (string &key
                       (object-type json-object-type)
                       (array-type json-array-type)
                       (key-type json-key-type))
  "[user] Read the JSON object from STRING."
  (let ((json-object-type object-type)
        (json-array-type array-type)
        (json-key-type key-type))
    (json-read-from-string string)))

;; simple
;; (defalias 'json-decode 'json-read-from-string)

;; Emacs 24.4 より json.el でも pretty-print が標準で利用可能になった
;; - M-x json-pretty-print-buffer ()
;; - M-x json-pretty-print (begin end)

(when (featurep 'nadvice)
  (defun json-pretty-print--use-hash-table(f &rest args)
    "[user] {}, null を区別する."
    (let ((json-object-type 'hash-table))
      (apply f args)))
  (advice-add 'json-pretty-print :around 'json-pretty-print--use-hash-table)
  ;;(advice-remove 'json-pretty-print 'json-pretty-print--use-hash-table)
  )

(defun json-pretty-string (json-string)
  "[user] Pretty format JSON-STRING."
  (with-temp-buffer
    (insert json-string)
    (json-pretty-print-buffer)
    (buffer-string)))

;; (require 'json-reformat)
(defun json-reformat-buffer ()
  "[user] Reformat JSON in the buffer."
  (interactive "*")
  (json-reformat-region (point-min) (point-max)))
