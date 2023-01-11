;;; config/cocoa-emacs-conf.el

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

;; [alt] <-> [command]
(custom-set-variables
 '(mac-option-modifier  'alt)
 '(mac-command-modifier 'meta))

;; 入力ソースが "U.S." の場合に有効
(define-key key-translation-map (kbd "¥") (kbd "\\"))

(global-set-key (kbd "C-¥") 'toggle-input-method)

(use-package emacs
  :if (eq window-system 'mac)
  :custom
  ;; ゴミ箱に入れる
  (delete-by-moving-to-trash t)
  (trash-directory "~/.Trash/")

  ;; FIXME: 黒背景に濃い青は見づらい (テーマを利用していれば無用の長物)
  (ansi-color-names-vector
   ["black" "red3" "green3" "yellow3"
    "royal blue"                        ; Original "blue2"
    "magenta3" "cyan3" "gray90"])

  ;;
  (ispell-program-name "aspell")
  (locate-command "mdfind")
  )

;; Environment Path (-> config/30-exec-path-from-shell.el)

;;(setf (getenv "PATH") (concat "/usr/local/bin" path-separator (getenv "PATH")))
;;(setf (getenv "PATH") (concat (expand-file-name "~/bin") path-separator (getenv "PATH")))
;;(setf (getenv "PS1") "[$?][\\u@\\h:\\w]\n\\$ ")
(setf (getenv "GIT_EDITOR") "/Applications/Emacs.app/Contents/MacOS/bin/emacsclient")

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
