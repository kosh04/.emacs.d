;;; config/cocoa-emacs-conf.el

;; [alt] <-> [command]
(custom-set-variables
 '(mac-option-modifier  'alt)
 '(mac-command-modifier 'meta))

;; 入力ソースが "U.S." の場合に有効
(define-key key-translation-map (kbd "¥") (kbd "\\"))

(global-set-key (kbd "C-¥") 'toggle-input-method)

;; ゴミ箱に入れる
(custom-set-variables
 '(delete-by-moving-to-trash t)
 '(trash-directory "~/.Trash/"))

;; Environment Path (-> config/30-exec-path-from-shell.el)

;;(setf (getenv "PATH") (concat "/usr/local/bin" path-separator (getenv "PATH")))
;;(setf (getenv "PATH") (concat (expand-file-name "~/bin") path-separator (getenv "PATH")))
;;(setf (getenv "PS1") "[$?][\\u@\\h:\\w]\n\\$ ")
(setf (getenv "GIT_EDITOR") "/Applications/Emacs.app/Contents/MacOS/bin/emacsclient")

(unless (executable-find "cask")
  (setf (getenv "PATH") (substitute-env-vars "$HOME/.cask/bin:$PATH")))

;; ../Makefile 利用時に役に立つかも
(setf (getenv "EMACS") (expand-file-name invocation-name invocation-directory))

(custom-set-variables
 ;; FIXME
 '(ispell-program-name "aspell")
 '(locate-command "mdfind")
 )

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

(when (eq window-system 'mac)
  ;; プレフィックスキー (C-x,M-g) 入力時に IME をオフにして誤爆を防ぐ
  (mac-auto-ascii-mode +1))
