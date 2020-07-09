;;; config/Help,Manual,Documentation

(use-package emacs
  :custom
  ;; *Help* などのドキュメントに関する特殊バッファは
  ;; ウィンドウ表示時にフォーカスさせる (q 押下で quit)
  (help-window-select 't))

(use-package man
  :custom
  (Man-notify-method 'aggressive))

