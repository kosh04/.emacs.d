;;; config/Eshell

;; (global-set-key (kbd "C-c s") 'eshell)

(custom-set-variables
 '(eshell-ask-to-save-last-dir nil)
 '(eshell-cmpl-ignore-case t))

(with-eval-after-load 'eshell
  ;;(require 'em-smart)
  ;;(add-hook 'eshell-mode-hook 'eshell-smart-initialize)
  )

(with-eval-after-load 'em-term
  ;; 端末操作が必要なコマンドをterm.elに丸投げする
  (add-to-list 'eshell-visual-commands "ssh")
  (add-to-list 'eshell-visual-subcommands '("git" "log" "diff" "show" "grep")))
