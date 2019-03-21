;;; config/Game

(declare-function 'ansi-color-apply "ansi-color")

(defun telnet--ansi-filter (f proc string)
  (setq string (thread-last string
                 (ansi-color-apply)
                 (replace-regexp-in-string "\C-m" "\n")))
  ;;(message "TELNET FILTER: string=%S" string)
  (funcall f proc string))

(advice-add 'telnet-filter :around 'telnet--ansi-filter)
;; (advice-remove 'telnet-filter 'telnet--ansi-filter)

;; これよりも `term' + "telnet nethack.alt.org" の方が安定している..
(defun nethack ()
  "Run nethack (NOT WORKED YET)."
  (interactive)
  (telnet "nethack.alt.org"))
