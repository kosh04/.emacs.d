;;; config/Help,Manual,Documentation


(use-package help
  :ensure nil
  :custom
  ;; *Help* などのドキュメントに関する特殊バッファは
  ;; ウィンドウ表示時にフォーカスさせる (q 押下で quit)
  (help-window-select 't))

(use-package man
  :ensure nil
  :custom
  (Man-notify-method 'aggressive))

