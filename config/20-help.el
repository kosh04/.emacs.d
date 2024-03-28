;;; config/Help,Manual,Documentation

(use-package emacs ;; help?
  :custom
  ;; *Help* などのドキュメントに関する特殊バッファは
  ;; ウィンドウ表示時にフォーカスさせる (q 押下で quit)
  (help-window-select t)
  ;; describe-bindings (<help> b) のカテゴリ分けを少し賢く
  (describe-bindings-outline t))

(use-package man
  :custom
  (Man-notify-method 'aggressive))

(use-package helpful :disabled)
