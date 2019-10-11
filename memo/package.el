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

;; Pallet - A package management tool for Emacs, built on Cask.
;; https://github.com/rdallasgray/pallet

;; コマンドライン以外から読み込んでも多分意味ないのでは…？
;; -> `cask exec emacs ...' と同じ効果が期待できる（のかもしれない）
(use-package cask
  :defer t
  :mode ("Cask\\'" . emacs-lisp-mode)
  :config (cask-initialize))

;; ~/.emacs.d./Cask を同期させる
(use-package pallet
  ;;:defer t
  :config (pallet-mode t)
  :ensure cask)

;; *** GNU elpa の署名がどこかに消えたときの対処法
;; $ curl -O https://elpa.gnu.org//packages/archive-contents
;; $ curl -O https://elpa.gnu.org//packages/archive-contents.sig
;; $ gpg --verify archive-contents.sig
;; gpg: assuming signed data in 'archive-contents'
;; gpg: Signature made 09/27/19 06:10:02
;; gpg:                using RSA key C433554766D3DDC64221BFAA066DAFCB81E42C40
;; gpg: Can't check signature: No public key
;; $ gpg --homedir $HOME/.emacs.d/elpa/gnupg --search-key C433554766D3DDC64221BFAA066DAFCB81E42C40
;; gpg: data source: http://pgpkeys.eu:11371
;; (1)     GNU ELPA Signing Agent (2019) <elpasign@elpa.gnu.org>
;;           3072 bit RSA key 066DAFCB81E42C40, created: 2019-04-23, expires: 2024-04-21
;; Keys 1-1 of 1 for "C433554766D3DDC64221BFAA066DAFCB81E42C40".  Enter number(s), N)ext, or Q)uit > 1
;; gpg: key 066DAFCB81E42C40: public key "GNU ELPA Signing Agent (2019) <elpasign@elpa.gnu.org>" imported
;; gpg: Total number processed: 1
;; gpg:               imported: 1
;; $ gpg --homedir $HOME/.emacs.d/elpa/gnupg --verify archive-contents.sig
;; gpg: assuming signed data in 'archive-contents'
;; gpg: Signature made 09/27/19 06:10:02
;; gpg:                using RSA key C433554766D3DDC64221BFAA066DAFCB81E42C40
;; gpg: Good signature from "GNU ELPA Signing Agent (2019) <elpasign@elpa.gnu.org>" [unknown]
;; gpg: WARNING: This key is not certified with a trusted signature!
;; gpg:          There is no indication that the signature belongs to the owner.
;; Primary key fingerprint: C433 5547 66D3 DDC6 4221  BFAA 066D AFCB 81E4 2C40
