;;; config/disabled

(put 'upcase-region 'disabled nil)    ; C-x C-u
(put 'downcase-region 'disabled nil)  ; C-x C-l
;(put 'scroll-left 'disabled nil)      ; C-x <
(put 'narrow-to-region 'disabled nil) ; C-x n n
(put 'narrow-to-page 'disabled nil)   ; C-x n p
(put 'set-goal-column 'disabled nil)  ; C-x C-n
(put 'erase-buffer 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil) ; [dired] a

;; (setq disabled-command-function nil)
