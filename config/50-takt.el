;;; conf/Takt -- Text-based Music Programming Tools

;; http://takt.sourceforge.net/

(use-package takt-mode
  :if (eq window-system 'w32)
  :load-path "C:/Program Files (x86)/Takt/share/emacs/site-lisp/"
  :mode ("\\.takt\\'" . takt-mode)
  :config
  (add-to-list 'exec-path "C:/Program Files (x86)/Takt/bin"))
