;;; config/Python

;; https://www.emacswiki.org/emacs/ProgrammingWithPythonModeDotEl

(cl-case system-type
  (darwin (custom-set-variables
           '(python-shell-interpreter "/usr/local/bin/python3"))))

(add-hook 'python-mode-hook 'eldoc-mode t)
(add-hook 'python-mode-hook 'company-mode t)

;; PEP 0008 -- Style Guide for Python Code
;; https://www.python.org/dev/peps/pep-0008/
(use-package py-autopep8
  :defer t
  :init (add-hook 'python-mode-hook 'py-autopep8-enable-on-save))