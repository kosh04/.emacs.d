;;; memo/progmode.el

;;; カーネル編集用 C-mode
(defun linux-c-mode ()
  "C mode with adjusted defaults for use with the Linux kernel"
  (interactive)
  (c-mode)
  (c-set-style "K&R")
  (setq c-indent-level 8)
  (setq c-brace-imaginary-offset 0)
  (setq c-brace-offset -8)
  (setq c-argdecl-indent 8)
  (setq c-label-offset -8)
  (setq c-continued-statement-offset 8)
  (setq indent-tabs-mode t)
  (setq tab-width 8)
  (setq c-basic-offset 8)
  t)
(add-to-list 'auto-mode-alist '("/usr/src/linux.*/.*\\.[ch]$" . linux-c-mode))

;; インデント関係
c-style-alist
(c-set-style "gnu")
(c-set-style "stroustrup")
(c-set-style "whitesmith")
(c-set-style "linux")

;; offsetに渡すシンボル値の意味
;; c-basic-offsetの...
;; +    1 倍
;; -   -1 倍
;; ++   2 倍
;; --  -2 倍
;; *  0.5 倍
;; / -0.5 倍

;; C-c C-s: c-show-syntactic-information
;; 現在のポイントの構文情報を表示

;; モード名をもっと短く
(add-hook 'emacs-lisp-mode-hook
          #'(lambda () (setq mode-name "Elisp")))

;; 現在の関数名をモードラインに表示 (M-x: which-function-mode)
(which-function-mode t)