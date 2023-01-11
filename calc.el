;;; calc.el: `calc-settings-file'

;; TODO: (use-package calc :custom ("..."))
(require 'calc)
(require 'calc-units)

;;; Custom units stored by Calc on Tue Apr  5 03:39:35 2016
(setq math-additional-units '(
  (bit nil "Bit")
  (byte "8 * bit" "Byte")
  (bps "bit / s" "Bit per Second")

  (B nil "Byte")
  (KiB "1024 * B"   "Kilo Byte")
  (MiB "1024 * KiB" "Mega Byte")
  (GiB "1024 * MiB" "Giga Byte")

  ;; (b "B / 8" "Bit")
  ;; (Kib "1024 * b" "Kilo Bit")
  ;; (Mib "1024 * Kib" "Mega Bit")
  ;; (Gib "1024 * Mib" "Giga Bit")
  ))
;;; End of custom units

