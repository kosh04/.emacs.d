;;; config/cocoa-emacs-conf.el

;; XXX: current-language-environment=English on emacsformacosx @28.2
;;(set-language-environment "UTF-8")
(or (equal current-language-environment "Japanese")
    (set-language-environment "Japanese"))

;; NOTE: Mac における Emacs バイナリいろいろ
;; - Emacs for Mac OS X
;;   https://emacsformacosx.com/
;;   window-system=ns
;;   公式ソースからビルドするのとほぼ同じ構成
;; - emacs-mac port by Mitsuharu Yamamoto (EMP)
;;   https://bitbucket.org/mituharu/emacs-mac/overview
;;   window-system=mac
;;   Homebrew 経由のインストールも可能 https://github.com/railwaycat/homebrew-emacsmacport

;; タイトルバー透過
;(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
;(add-to-list 'default-frame-alist '(ns-appearance . dark))

(custom-set-variables
 ;; [alt] <-> [command]
 '(mac-option-modifier  'alt)
 '(mac-command-modifier 'meta)
 ;; mac-function-modifier
 ;; mac-control-modifier
 ;; mac-right-command-modifier
 ;; mac-right-control-modifier
 ;; mac-right-option-modifier
 )

;; 入力ソースが "U.S." の場合に有効
(define-key key-translation-map (kbd "¥") (kbd "\\"))

(global-set-key (kbd "C-¥") 'toggle-input-method)

(use-package emacs
  :if (member window-system '(mac ns))
  :custom
  ;; ゴミ箱に入れる
  (delete-by-moving-to-trash t)
  (trash-directory "~/.Trash/")
  ;;
  (ispell-program-name "aspell")
  (locate-command "mdfind")
  )

;; Environment Path (-> config/30-exec-path-from-shell.el)

;;(setf (getenv "PATH") (concat "/usr/local/bin" path-separator (getenv "PATH")))
;;(setf (getenv "PATH") (concat (expand-file-name "~/bin") path-separator (getenv "PATH")))
;;(setf (getenv "PS1") "[$?][\\u@\\h:\\w]\n\\$ ")
(setf (getenv "GIT_EDITOR") "/Applications/Emacs.app/Contents/MacOS/bin/emacsclient")

'
(unless (executable-find "cask")
  (setf (getenv "PATH") (substitute-env-vars "$HOME/.cask/bin:$PATH")))

;; ../Makefile 利用時に役に立つかも
(setf (getenv "EMACS") (expand-file-name invocation-name invocation-directory))

;; Tramp
;; リモートファイルを開く時に`vc-svn-registered'が邪魔をする場合がある
;;(setq vc-handled-backends nil)
;;(setq vc-handled-backends '(RCS CVS SVN SCCS SRC Bzr Git Hg Mtn))
;;(setq vc-handled-backends '(Git))

(add-to-list 'completion-ignored-extensions ".dSYM")

(with-eval-after-load 'dired-x
  (add-to-list 'dired-guess-shell-alist-user '("\\.ai\\'" "qlmanage -p")))

(use-package apples-mode
  :mode ("\\.\\(applescri\\|sc\\)pt\\'"  . apples-mode))

;; プレフィックスキー (C-x,M-g) 入力時に IME をオフにして誤爆を防ぐ
(if (fboundp 'mac-auto-ascii-mode)
    (mac-auto-ascii-mode +1))

;; Use ligatures
;; https://github.com/tonsky/FiraCode/wiki/Emacs-instructions#using-composition-mode-in-emacs-mac-port
(if (fboundp 'mac-auto-operator-composition-mode)
    (funcall 'mac-auto-operator-composition-mode))

;; (if (eq window-system 'ns) )
(setq ns-use-thin-smoothing t)

(menu-bar-mode +1)
;(setq visible-bell (not visible-bell))

;; コピペ: pbcopy pbpaste
