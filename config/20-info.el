;;; config/Info

(with-eval-after-load 'info
  (bind-keys :map Info-mode-map
             ("<M-left>"  . Info-history-back)
             ("<M-right>" . Info-history-forward)))
