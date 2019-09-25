;;; config/diff

(with-eval-after-load 'diff
  (setq diff-switches "-waru"))

;; Ediff
(with-eval-after-load 'ediff
  (custom-set-variables
   '(;; コントロール用のバッファを同一フレーム内に表示
     (ediff-window-setup-function 'ediff-setup-windows-plain)
     ;; diffのバッファを上下ではなく左右に並べる
     (ediff-split-window-function 'split-window-horizontally)
     )))
