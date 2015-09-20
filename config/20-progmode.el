;;; config/progmode.el

;; スペースでインデント
(setq-default indent-tabs-mode nil)

;; コメント
(setq comment-style 'multi-line)

;; 自動改行と欲張りな削除
(add-hook 'c-mode-common-hook 'c-toggle-auto-hungry-state)

(require 'electric)

(electric-pair-mode +1)
(electric-layout-mode +1)

;;(add-to-list 'electric-pair-pairs '(?< . ?>))
(add-to-list 'electric-pair-pairs '(?{ . ?}))

;;(add-to-list 'electric-layout-rules '(?{ . around))

;; 関数一覧
(require 'imenu)
(global-set-key (kbd "C-c C-l") 'imenu)
(add-hook 'emacs-lisp-mode-hook #'imenu-add-menubar-index)

;; flyspell
(add-hook 'c-mode-hook 'flyspell-prog-mode)

(defun indent-and-next-line (&optional args)
  "インデントして次の行へ."
  (interactive "p")
  (dotimes (x args)
    (indent-according-to-mode)
    (line-move 1)))

(global-set-key (kbd "M-n") 'indent-and-next-line)

;; PHP
(use-package php-mode
  :defer t
  :config
  (setq php-search-url "http://www.php.net/ja/")        ; C-c C-f
  (setq php-manual-url "http://www.php.net/manual/ja/") ; C-c C-m
  (setq php-mode-force-pear t)
  (bind-keys :map php-mode-map ("RET" . newline-and-indent))
  (defun php-user-hook ()
    (define-abbrev php-mode-abbrev-table "ex" "extends")
    ;; (setq comment-start "// " comment-end "")
    (c-set-offset 'arglist-intro '+) ; array(...) インデントをスマートにする
    (c-set-offset 'arglist-close 0)
    t)
  (add-hook 'php-mode-hook #'php-user-hook)
  :ensure nil)

;; Batch mode
(use-package batch-mode  
  :defer t
  :mode ("\\.bat\\'"
         "\\.cmd\\'"))

;; newLISP
(use-package newlisp-mode
  :defer t
  :config
  (progn
    (bind-keys :map newlisp-mode-map ("C-c h" . newlisp-lookup-manual))
    (setq newlisp-manual-text "~/Dropbox/Public/newlisp/newlisp_manual.txt"))
  :ensure t)

;; TypeScript
(use-package tss
  :defer t
  :mode ("\\.ts\\'" . typescrip-mode))

;; Wandbox
(use-package wandbox
  :defer t
  :bind (("C-c w w" . wandbox)
         ("C-c w e" . wandbox-eval-last-sexp)
         ("C-c w l" . wandbox-list-compilers)))

;; Smart Compile
(use-package smart-compile
  :defer t
  ;; :bind ("C-c c" . smart-compile)
  )
