;;; config/Scheme

;; (defun run-guile ()
;;   "[user] Run guile interpreter."
;;   (interactive)
;;   (run-scheme "guile"))

;; Scheme 開発環境
;; https://github.com/jaor/geiser
(use-package geiser
  :custom
  (geiser-active-implementations '(guile))
  (geiser-repl-history-filename (locate-user-emacs-file ".geiser_history")))

;; Gauche
;; or https://github.com/mhayashi1120/Emacs-gosh-mode
(when (executable-find "gosh")
  (custom-set-variables
   '(scheme-program-name "gosh")))

(defalias 'run-gauche 'run-scheme)
