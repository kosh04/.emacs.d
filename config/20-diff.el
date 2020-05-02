;;; config/Diff

(use-package diff
  :custom
  (diff-switches "-waru"))

(use-package ediff
  :custom
  ;; コントロール用のバッファを同一フレーム内に表示
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  ;; diffのバッファを上下ではなく左右に並べる
  (ediff-split-window-function 'split-window-horizontally)
  )
