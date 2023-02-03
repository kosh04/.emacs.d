;;; config/Perl

;; prefer CPerl
(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("\\(?:mini\\)?perl[5]?" . cperl-mode))

(set-variable 'cperl-hairy t)

(use-package perl6-mode :disabled)
