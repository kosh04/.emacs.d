;;; config/wandbox

(use-package wandbox
  :defer t
  ;; 開発版
  :load-path "~/Documents/GitHub/emacs-wandbox/"
  :pin #:manual
  :bind (("C-c w w" . wandbox)
         ("C-c w e" . wandbox-eval-last-sexp)
         ("C-c w i" . wandbox-insert-template)
         ("C-c w l" . wandbox-list-compilers))
  :config
  ;; 通信環境の問題でHTTPSがたまに失敗することがある
  (ignore-errors
    (wandbox-add-server "fetus" "https://wandbox.fetus.jp")))
