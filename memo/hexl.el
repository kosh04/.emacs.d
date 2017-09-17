;;; Memo/Hexl

;; バッファが書き込み禁止ならSPCキーなどで移動したい
(add-hook ' hexl-mode-hook
          (lambda ()
            (add-hook 'read-only-mode-hook
                      (lambda ()
                        (view-mode (if buffer-read-only +1 -1)))
                      nil t)))
