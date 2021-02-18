;;; config/Python

;; https://www.emacswiki.org/emacs/PythonProgrammingInEmacs
;; https://www.emacswiki.org/emacs/ProgrammingWithPythonModeDotEl

;; (set-variable 'python-check-command (executable-find "flake8"))

(use-package python
  :hook ((python-mode . which-function-mode)
         (python-mode . eldoc-mode)
         (python-mode . company-mode))
  :custom
  (python-shell-interpreter "python3")
  ;; TODO: Emacs25.1@darwin does not work
  (python-shell-completion-native-enable nil)
  ;; M-x pdb (pdb3)
  (gud-pdb-command-name "python3 -m pdb")
  :config
  (unless (getenv "PYTHONIOENCODING")
    (setf (getenv "PYTHONIOENCODING") "utf-8"))
  )

;; PEP 0008 -- Style Guide for Python Code
;; https://www.python.org/dev/peps/pep-0008/
;; $ pip install --upgrade autopep8
(use-package py-autopep8
  :after python
  :hook (python-mode . py-autopep8-enable-on-save)
  :custom
  (py-autopep8-options '("--max-line-length=100"))
  (flycheck-flake8-maximum-line-length 100)
  ;;:config
  ;;(setf (getenv "PYFLAKES_NODOCTEST") "true")
  )

(with-eval-after-load 'flycheck
  (setq-default flycheck-python-flake8-executable "flake8"))

;;(define-key python-mode-map (kbd "C-c m d") #'pydoc)
