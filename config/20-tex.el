;;; config/tex.el

;; LaTeX
(with-eval-after-load 'tex-mode
  (setq latex-run-command "platex -kanji=euc")
  (add-to-list 'tex-compile-commands '("platex %f" "%f" "%f.dvi"))
  (setq tex-dvi-view-command "xdvi")
  (add-to-list 'auto-coding-alist '("\\.tex$" . japanese-iso-8bit))
  (add-to-list 'completion-ignored-extensions ".aux")
  (add-to-list 'completion-ignored-extensions ".dvi")
  ;; (add-to-list 'completion-ignored-extensions ".log")
  (add-hook 'latex-mode-hook 'prettify-symbols-mode))

