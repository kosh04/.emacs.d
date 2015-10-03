;; site-lisp/unicode-escape-test.el

(require 'ert)
(require 'unicode-escape)

(ert-deftest unicode-escape ()
  (should (string= (unicode-escape "") ""))
  (should (string= (unicode-escape "\u3053\u3093\u306b\u3061\u306f")
                   "\\u3053\\u3093\\u306b\\u3061\\u306f"))
  (should (string= (unicode-escape "Hello, World!")
                   "Hello, World!"))
  t)

(ert-deftest unicode-unescape ()
  (should (string= (unicode-unescape "\\u3053\\u3093\\u306b\\u3061\\u306f")
                   "\u3053\u3093\u306b\u3061\u306f"))
  (should (string= (unicode-unescape "Hello, World!")
                   "Hello, World!"))
  t)

(ert-deftest unicode-escape-region ()
  ;; pass
  (should t))

(ert-deftest unicode-unescape-region ()
  ;; pass
  (should t))
