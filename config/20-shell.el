;;; config/Shell

(defun user:shell-other-window ()
  "シェルを別ウィンドウで開きます."
  (interactive)
  (let ((buffer (save-window-excursion
                  ;; Enable current-prefix-arg
                  (call-interactively 'shell))))
    ;; FIXME: 分割済みウィンドウでは機能していない
    (or (eq buffer (current-buffer))
        (switch-to-buffer-other-window buffer))))

(global-set-key (kbd "C-c s") 'user:shell-other-window)

;; エスケープシーケンスを処理する ("ls --color" が使える)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(defun user/shell-mode-hook ()
  (setq show-trailing-whitespace nil))

;;(add-hook 'shell-mode-hook 'user/shell-mode-hook)

;; shell-command(M-!) のコマンド入力に補完を効かせる
;; http://namazu.org/~tsuchiya/
(use-package "shell-command"
  :disabled t
  :config (shell-command-completion-mode t))

(with-eval-after-load 'shell
  ;; 上下キーで補完したい
  (define-key shell-mode-map (kbd "<up>")   'comint-previous-input)
  (define-key shell-mode-map (kbd "<down>") 'comint-next-input)
  t)

;; 端末内でlessが使えたりする反面、シェル内での編集に向かない
(defun user:ansi-term ()
  "端末エミュレータを開きます."
  (interactive)
  (ansi-term shell-file-name))
