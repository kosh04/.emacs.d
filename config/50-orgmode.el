;;; config/Org

(use-package org
  :hook
  (org-mode . outline-show-all)
  (org-mode . which-function-mode)
  :config
  (defun org-mode/which-func-functions ()
    (setq-local which-func-functions
                (lambda ()
                  (string-join (org-get-outline-path t) " > "))))
  (add-hook 'org-mode-hook 'org-mode/which-func-functions)
  )
