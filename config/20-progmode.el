;;; config/progmode

;; スペースでインデント
(setq-default indent-tabs-mode nil)

;; コメント
(setq comment-style 'multi-line)

;; 自動改行と欲張りな削除
;; (add-hook 'c-mode-common-hook 'c-toggle-auto-hungry-state)

(require 'electric)

(electric-pair-mode +1)
(electric-layout-mode +1)
;;(electric-indent-mode -1)

;;(add-to-list 'electric-pair-pairs '(?< . ?>))
(add-to-list 'electric-pair-pairs '(?{ . ?}))

;;(add-to-list 'electric-layout-rules '(?{ . around))

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

;; Batch mode
(use-package batch-mode  
  :defer t
  :mode ("\\.bat\\'"
         "\\.cmd\\'"))

;; Smart Compile
(use-package smart-compile
  :defer t
  ;; :bind ("C-c c" . smart-compile)
  )
