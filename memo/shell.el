;;; memo/Shell

;; shell-command(M-!) のコマンド入力に補完を効かせる
;; http://namazu.org/~tsuchiya/
(use-package "shell-command"
  :config (shell-command-completion-mode t))
