;;; config/prolog.el

(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode) t)
(add-to-list 'auto-mode-alist '("\\.swi\\'" . prolog-mode) t)

(custom-set-variables
 '(prolog-system 'swi))
