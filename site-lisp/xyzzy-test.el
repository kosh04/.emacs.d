;;; site-lisp/xyzzy-test.el

(require 'ert)
(require 'xyzzy)

(ert-deftest sub-directory-p ()
  (should (sub-directory-p "" ""))
  (should (sub-directory-p "/" "/"))
  (should (sub-directory-p "c:/windows/system" "c:/windows"))
  (should (sub-directory-p "c:/WINDOWS/system" "c:/windows")) ; case-fold=t
  (should (sub-directory-p "~/.emacs.d/" "~/.emacs.d/"))
  (should (sub-directory-p "~/.emacs.d/" "~/.emacs.d"))
  (should (sub-directory-p "~/.emacs.d" "~/.emacs.d"))
  (should-not (sub-directory-p "/" "/bin"))
  (should-not (sub-directory-p "/bin" "/usr/bin"))
  (should-not (sub-directory-p "/usr/bin" "/usr/sbin"))
  (should-not (sub-directory-p "/usr/local/bin" "/usr/bin"))
  (should-error (sub-directory-p nil "")))
