;;; config/wandbox.el

;; 開発版
(add-to-list 'load-path "~/Documents/GitHub/emacs-wandbox/")

(use-package wandbox
  :defer t
  :bind (("C-c w w" . wandbox)
         ("C-c w e" . wandbox-eval-last-sexp)
         ("C-c w l" . wandbox-list-compilers))
  :config
  (wandbox-add-server "fetus" "https://wandbox.fetus.jp"))
