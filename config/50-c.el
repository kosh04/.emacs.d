;;; config/C

(use-package c-eldoc
  :defer t
  :init (add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
  :config
  (custom-set-variables
   '(c-eldoc-cpp-command "cpp")
   '(c-eldoc-includes "-I. -I..")))
