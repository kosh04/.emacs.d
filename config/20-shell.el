;;; config/shell.el

(defun my:shell-other-window ()
  (interactive)
  (let ((buffer (save-window-excursion
                  ;; Enable current-prefix-arg
                  (call-interactively 'shell))))
    (or (eq buffer (current-buffer))
        (switch-to-buffer-other-window buffer))))

(global-set-key (kbd "C-c s") 'my:shell-other-window)

;; エスケープシーケンスを処理する ("ls --color" が使える)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; shell-command(M-!) のコマンド入力に補完を効かせる
;; http://namazu.org/~tsuchiya/
(use-package "shell-command"
  :init (shell-command-completion-mode t)
  :ensure nil)

(with-eval-after-load 'shell
  ;; 上下キーで補完したい
  (define-key shell-mode-map (kbd "<up>")   'comint-previous-input)
  (define-key shell-mode-map (kbd "<down>") 'comint-next-input)
  t)

;; Eshell
(with-eval-after-load 'eshell
  ;; (add-hook 'eshell-mode-hook 'eshell-user-setup-hook)
  (setq eshell-ask-to-save-last-dir nil)
  t)
