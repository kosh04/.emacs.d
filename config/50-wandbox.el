;;; config/wandbox.el

;; 開発版
;;(add-to-list 'load-path "~/Documents/GitHub/emacs-wandbox/")
(add-to-list 'load-path "~/src/gitrepo/emacs-wandbox/")

(use-package wandbox
  :defer t
  :bind (("C-c w w" . wandbox)
         ("C-c w e" . wandbox-eval-last-sexp)
         ("C-c w l" . wandbox-list-compilers))
  :config
  ;; 環境の問題でHTTPS通信がたまに失敗することがある
  (ignore-errors
    (wandbox-add-server "fetus" "https://wandbox.fetus.jp")))
