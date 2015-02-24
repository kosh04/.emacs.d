;;; config/progmode.el

;; スペースでインデント
(setq-default indent-tabs-mode nil)

;; コメント
(setq comment-style 'multi-line)

;; 自動改行と欲張りな削除
(add-hook 'c-mode-common-hook 'c-toggle-auto-hungry-state)

;; 関数一覧
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

;; PHP
(use-package php-mode
  :init
  (progn
    (define-key php-mode-map (kbd "RET") #'newline-and-indent)
    (defun php-user-hook ()
      (define-abbrev php-mode-abbrev-table "ex" "extends")
      ;; (setq comment-start "// " comment-end "")
      (c-set-offset 'arglist-intro '+) ; array(...) インデントをスマートにする
      (c-set-offset 'arglist-close 0)
      t)
    (add-hook 'php-mode-hook #'php-user-hook))
  :ensure nil)

;; newLISP
(use-package newlisp-mode
  :init
  (progn
    (bind-key "C-c h" 'newlisp-lookup-manual newlisp-mode-map)
    (setq newlisp-manual-text "~/Dropbox/Public/newlisp/newlisp_manual.txt"))
  :ensure t)

(use-package wandbox
  :bind (("C-c w w" . wandbox)
         ("C-c w e" . wandbox-eval-last-sexp))
  :ensure t)
