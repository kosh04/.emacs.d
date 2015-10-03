;;; config/json.el

(require 'json)

(cl-defun json-decode (string &key
                       (object-type json-object-type)
                       (array-type json-array-type)
                       (key-type json-key-type))
  "Read the JSON object from STRING."
  (let ((json-object-type object-type)
        (json-array-type array-type)
        (json-key-type key-type))
    (json-read-from-string string)))

;; simple
;; (defalias 'json-decode 'json-read-from-string)
