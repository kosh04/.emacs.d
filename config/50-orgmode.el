;;; config/Org-mode

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

(with-eval-after-load 'find-file
  ;; [init.el 読書会用] config.org <-> config.el を行来する
  (add-to-list 'cc-other-file-alist '("\\.org\\'" (".el")))
  (add-to-list 'cc-other-file-alist '("\\.el\\'" (".org"))))

(defun dired-tangle (filename)
  "[user] Dired 内で *.org -> *.el の生成."
  (interactive (list (dired-get-filename)))
  (org-babel-tangle-file filename))

(use-package org-bullets
  :disabled
  :hook (org-mode . org-bullets-mode))

(use-package org-modern
  :after org
  :config (global-org-modern-mode +1))
