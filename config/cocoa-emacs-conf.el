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
;;(setf (getenv "PS1") "[\\u@\\h:\\w]\\$ ")

;; ../Makefile 利用時に役に立つかも
(setf (getenv "EMACS") (expand-file-name invocation-name invocation-directory))

;; Font
(set-face-attribute 'default nil :family "Menlo" :height 150)
;;(set-fontset-font nil 'unicode (font-spec :family "Menlo"))
;;(set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Hiragino Kaku Gothic ProN"))
;;(set-fontset-font (frame-parameter nil 'font) 'japanese-jisx0208 `("Hiragino Maru Gothic Pro" . "iso10646-1"))

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

(use-package apples-mode
  :mode ("\\.\\(applescri\\|sc\\)pt\\'"  . apples-mode))
