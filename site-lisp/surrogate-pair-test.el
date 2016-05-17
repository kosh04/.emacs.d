;;; surrogate-pair-test.el

(require 'surrogate-pair)
(require 'cl-lib)
(require 'ert)

(defvar surrogate-pair-test-pairs
  '((?\U00010000 . [?\uD800 ?\uDC00])
    (?\U00010001 . [?\uD800 ?\uDC01])
    (?\U00010401 . [?\uD801 ?\uDC01])
    (?\U00010E6D . [?\uD803 ?\uDE6D])
    (?\U0001D11E . [?\uD834 ?\uDD1E])
    (?\U0010FFFF . [?\uDBFF ?\uDFFF]))
  "List of set (code-point . surrogate-pair).
Type is (Char . Vector[Char Char]).")

(ert-deftest surrogate-pair ()
  (cl-loop for data in surrogate-pair-test-pairs
           do (cl-destructuring-bind (char . pair) data
                (should (equal      (encode-utf-16-pair char) pair))
                (should (char-equal (decode-utf-16-pair pair) char)))))

(ert-deftest surrogate-pair-convert ()
  (should (string= (surrogate-pair-encode "SUSHI(寿司)=\U0001f363")
                   "SUSHI(寿司)=\uD83C\uDF63"))
  (should (string= (surrogate-pair-decode "SUSHI(寿司)=\uD83C\uDF63")
                   "SUSHI(寿司)=\U0001f363")))

(unless noninteractive
  (ert t))

;;; surrogate-pair-test.el ends here.
