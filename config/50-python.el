;;; config/Python

;; https://www.emacswiki.org/emacs/PythonProgrammingInEmacs
;; https://www.emacswiki.org/emacs/ProgrammingWithPythonModeDotEl

;; (set-variable 'python-check-command (executable-find "flake8"))

(cl-case system-type
  (darwin (custom-set-variables
           '(python-shell-interpreter "/usr/local/bin/python3")
           ;; TODO: Emacs25.1 does not work
           '(python-shell-completion-native-enable nil)
           ;; M-x pdb
           '(gud-pdb-command-name "python3 -m pdb"))))

(unless (getenv "PYTHONIOENCODING")
  (setf (getenv "PYTHONIOENCODING") "utf-8"))

(add-hook 'python-mode-hook 'eldoc-mode t)
(add-hook 'python-mode-hook 'company-mode t)

;; PEP 0008 -- Style Guide for Python Code
;; https://www.python.org/dev/peps/pep-0008/
;; $ pip install --upgrade autopep8
(use-package py-autopep8
  :after python
  :init (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
  :config (custom-set-variables
           '(py-autopep8-options '("--max-line-length=100"))
           '(flycheck-flake8-maximum-line-length 100))
  ;;(setf (getenv "PYFLAKES_NODOCTEST") "true")
  )
