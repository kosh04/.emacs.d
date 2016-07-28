;;; memo/package.el

;; パッケージのインストールはセキュリティ上 HTTPS を推奨するという話
;; Your Text Editor Is Malware
;; https://glyph.twistedmatrix.com/2015/11/editor-malware.html

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))

;; インストール元を固定する. 安定版を利用したいときに.
(setq package-pinned-packages
      '((cider . "melpa-stable")
        (magit . "melpa-stable")
        (elnode . "marmalade")))

;; バージョンを固定する. それ以外のバージョンは無視される
(add-to-list 'package-load-list '(muse "3.20"))

;; Cask - Project management for Emacs package development
(require 'cask "~/.cask/cask.el" t)

;; コマンドライン以外から読み込んでも多分意味ないのでは…？
(use-package cask
  :defer t
  :mode ("Cask\\'" . emacs-lisp-mode)
  :config (cask-initialize))

;; ~/.emacs.d./Cask を同期させる
(use-package pallet
  ;;:defer t
  :config (pallet-mode t)
  :ensure cask)
