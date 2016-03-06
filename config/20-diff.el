;;; confif/diff.el

(custom-set-variables
 '(diff-switches "-waru"))

;; Ediff
(with-eval-after-load 'ediff
  ;; コントロール用のバッファを同一フレーム内に表示
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  ;; diffのバッファを上下ではなく左右に並べる
  (setq ediff-split-window-function 'split-window-horizontally))
