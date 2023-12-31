;; emacs-28 (or later) features

(unless (version<= "28.1" emacs-version)
  (warn "your emacs-%s" emacs-version))

;;-> (fset 'yes-or-no-p #'y-or-n-p)
(setq use-short-answers t)

(use-package emacs
  :disabled
  :when
  (and (fboundp 'native-comp-available-p)
       (native-comp-available-p))
  :config
  (setq native-comp-deferred-compilation t)
  (setq native-comp-async-query-on-exit t)
  (setq native-comp-async-jobs-number 4)
  (setq native-comp-async-report-warnings-errors nil)

  (setq package-native-compile t)
  )
