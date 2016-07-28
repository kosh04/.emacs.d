;;; config/GDB

(with-eval-after-load 'gdb-mi
  (gdb-many-windows t)
  ;;(setq gdb-use-separate-io-buffer t)
  ;;(setq gud-tooltip-echo-area t)
  (add-hook 'gdb-mode-hook #'gud-tooltip-mode))
