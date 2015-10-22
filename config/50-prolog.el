;;; config/prolog.el

(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))
(add-to-list 'auto-mode-alist '("\\.swi\\'" . prolog-mode))

(custom-set-variables
 '(prolog-system 'swi))
