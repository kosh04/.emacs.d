;;; memo/autopairs.el

;; 括弧()や引用符""を自動挿入するコマンドいろいろ

;; http://www.emacswiki.org/emacs/AutoPairs

;; 標準コマンド
(global-set-key (kbd "(") 'insert-pair)     ; ()
(global-set-key (kbd "[") 'insert-pair)     ; []
(global-set-key (kbd "{") 'insert-pair)     ; {}
(global-set-key (kbd "<") 'insert-pair)     ; <>
(global-set-key (kbd "\"") 'insert-pair)    ; ""
(global-set-key (kbd "ESC '") 'insert-pair) ; ''
(global-set-key (kbd "ESC `") 'insert-pair) ; `'

;; Skeleton Mode
(setq skeleton-pair t)
(global-set-key (kbd "(") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "[") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "{") 'skeleton-pair-insert-maybe)

(require 'electric)
(electric-pair-mode +1)
(electric-layout-mode +1)

(require 'autopair)
(autopair-global-mode)

(require 'paredit)

(require 'smartparens)

(require 'wrap-region)
