;;; config/C

(use-package c-eldoc
  :defer t
  :init (add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
  :config
  (custom-set-variables
   '(c-eldoc-cpp-command "cpp")
   '(c-eldoc-includes "-I. -I..")))

(use-package company-c-headers
  :defer t
  :init
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-c-headers))
  :ensure company)
