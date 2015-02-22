;;; config/progmode.el

;; 自動改行と欲張りな削除
(add-hook 'c-mode-common-hook 'c-toggle-auto-hungry-state)

;;; 関数一覧
(require 'imenu)
(global-set-key (kbd "C-c C-l") 'imenu)
(add-hook 'emacs-lisp-mode-hook #'imenu-add-menubar-index)

(defun indent-and-next-line (&optional args)
  "インデントして次の行へ."
  (interactive "p")
  (dotimes (x args)
    (indent-according-to-mode)
    (line-move 1)))

(global-set-key (kbd "M-n") 'indent-and-next-line)
