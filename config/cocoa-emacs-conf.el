;;; config/cocoa-emacs-conf.el

;; [alt] <-> [command]
(custom-set-variables
 '(mac-option-modifier  'alt)
 '(mac-command-modifier 'meta))

;; 入力ソースが "U.S." の場合に有効
(define-key key-translation-map (kbd "¥") (kbd "\\"))

(global-set-key (kbd "C-¥") 'toggle-input-method)

;; Path
(setf (getenv "PATH") (concat "/usr/local/bin" path-separator (getenv "PATH")))
;;(setf (getenv "PATH") (concat (expand-file-name "~/bin") path-separator (getenv "PATH")))

(setf (getenv "PS1") "[\\u@\\h:\\w]\\$ ")

;; $GOPATH / exe-path-from-shell でなんとかならんかね
(unless (getenv "GOPATH")
  (setf (getenv "GOPATH")
        (concat (expand-file-name "~/opt/go") path-separator
                (expand-file-name "~/prog/go"))))

(progn
  (dolist (path (parse-colon-path (getenv "GOPATH")))
    (add-to-list 'exec-path (expand-file-name "bin" path)))
  (setf (getenv "PATH") (mapconcat #'identity exec-path path-separator)))

;; Font
(set-face-attribute 'default nil :family "Menlo" :height 150)
;;(set-fontset-font nil 'unicode (font-spec :family "Menlo"))
;;(set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Hiragino Kaku Gothic ProN"))
;;(set-fontset-font (frame-parameter nil 'font) 'japanese-jisx0208 `("Hiragino Maru Gothic Pro" . "iso10646-1"))

;; FIXME
(custom-set-variables
 '(ispell-program-name "aspell"))

;; Tramp
;; リモートファイルを開く時に`vc-svn-registered'が邪魔をする場合がある
(setq vc-handled-backends nil)
;;(setq vc-handled-backends '(RCS CVS SVN SCCS SRC Bzr Git Hg Mtn))
;;(setq vc-handled-backends '(Git))
