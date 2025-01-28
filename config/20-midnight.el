;;; config/Midnight

;; 不要なバッファは定期的に掃除 (M-x clean-buffer-list)
;; TODO: ウィンドウをフォーカス直後のプチフリーズの原因？

(require 'midnight)
(midnight-mode t)
(midnight-delay-set 'midnight-delay "3:30am")

(setq clean-buffer-list-delay-general 8) ; days
(setq clean-buffer-list-kill-buffer-names
      '("*Help*" "*Apropos*" "*Buffer List*" "*Compile-Log*" "*info*" "*vc*" "*vc-diff*" "*diff*"
        "*Completions*"
        "*Occur*"
        "*xref*"
        "*Shell Command Output*"
        "*Google Translate*"
        ))
