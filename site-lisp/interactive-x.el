;;; interactive-x.el --- Extend `interactive'        -*- lexical-binding: t; -*-

;; Copyright (C) 2017  KOBAYASHI Shigeru

;; Author: KOBAYASHI Shigeru <kosh@kosh-MBA.local>
;; Keywords: internal

;;; Commentary:

;; [TODO]

;;; Links:

;; - https://www.gnu.org/software/emacs/manual/html_node/elisp/Using-Interactive.html
;; - http://www.bookshelf.jp/texi/elisp-manual-20-2.5-jp/elisp_21.html
;; - emacs/src/callint.c:DEFUN("call-interactively", ...)

;;; Code:

(defmacro interactive* (&rest args)
  `(interactive (list ,@args)))

(defvar interactive-x-function-list
  '((?a . (completing-read "a " obarray 'fboundp))
    (?b . read-buffer)
    (?B . ??)
    (?c . read-char)
    (?C . read-command)
    (?d . point)
    (?D . read-directory-name)
    (?e . read-event)
    (?f . read-file-name)
    (?F . ??)
    (?G . ??)
    (?i . ignore)
    (?k . read-key-sequence)
    (?K . read-key-sequence-vector)
    (?U . ??)
    (?m . mark)                         ; ?
    (?M . (read-string "" nil nil nil t))
    (?n . read-number)
    (?N . ??)
    (?p . current-prefix-arg)           ; (prefix-numeric-value )
    (?P . prefix-arg)
    (?r . (list (region-beginning) (region-end)))
    (?s . read-string)
    (?S . (intern (read-string "")))
    (?v . read-variable)
    (?x . read-minibuffer)
    (?X . eval-minibuffer)
    (?z . read-coding-system)
    (?Z . read-non-nil-coding-system)

    (?+ . ignored)
    (?- . ignored)
    (?* . barf-if-buffer-read-only)
    (?@ . ??)
    (?^ . handle-shift-selection)
    ))

(defsubst interactive-x-parse (prompt)
  (cl-loop for (c . f) in interactive-x-function-list
           if (char-equal c (elt prompt 0))
           do :TODO))

;; Examples
(defun hello (&optional user)
  (interactive* "xTo: " ["A" "B" C] :predicate #'stringp)
  ;;=> (interactive (list (completing-read "To: " '("A" "B" C) #'stringp)))
  (message "Hello %s" user))

(provide 'interactive-x)
;;; interactive-x.el ends here
