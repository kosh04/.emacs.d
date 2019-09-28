;;; config/Org

(add-hook 'org-mode-hook 'outline-show-all)
(add-hook 'org-mode-hook 'which-func-mode)
;;(add-hook 'org-mode-hook 'view-mode)

(defun org-mode/which-func-functions ()
    (setq-local which-func-functions
                (lambda ()
                  (string-join (org-get-outline-path t) " > "))))
(add-hook 'org-mode-hook 'org-mode/which-func-functions)
