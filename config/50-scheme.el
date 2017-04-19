;;; config/Scheme

;; (defun run-guile ()
;;   "[user] Run guile interpreter."
;;   (interactive)
;;   (run-scheme "guile"))

;; Scheme 開発環境
;; https://github.com/jaor/geiser
(use-package geiser
  :config
  (custom-set-variables
   '(geiser-active-implementations '(guile))
   '(geiser-repl-history-filename (locate-user-emacs-file ".geiser_history"))))
