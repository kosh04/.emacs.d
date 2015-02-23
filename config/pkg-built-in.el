;;; config/pkg-built-in.el

;; 組み込みライブラリの設定
;; 複雑なものはファイル分けする (e.g. dired)

;; Ediff
(with-eval-after-load 'ediff
  ;; コントロール用のバッファを同一フレーム内に表示
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  ;; diffのバッファを上下ではなく左右に並べる
  (setq ediff-split-window-function 'split-window-horizontally))

;; JSON
(require 'json)
(defalias 'json-decode 'json-read-from-string)
