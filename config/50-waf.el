;;; config/Waf --- The Waf build system

;; https://bitbucket.org/dvalchuk/waf-mode

(use-package waf-mode
  :init
  (add-hook 'python-mode-hook   #'waf-conditionally-enable)
  (add-hook 'c++-mode-hook      #'waf-conditionally-enable)
  (add-hook 'c-mode-common-hook #'waf-conditionally-enable))
