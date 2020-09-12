;;; site-lisp/xyzzy-test.el

(require 'cl-lib)
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

(ert-deftest si:www-url-encode-decode ()
  (cl-loop
   for (str . encoded)
   in
   '(("あいうえお" . "%E3%81%82%E3%81%84%E3%81%86%E3%81%88%E3%81%8A")
     ("console.log('\U0001F363');" . "console.log('%F0%9F%8D%A3')%3B"))
   do
   (should (string= (si:www-url-encode str) encoded))
   (should (string= (si:www-url-decode encoded) str)))
  )
