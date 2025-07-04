;;; config/package

(require 'package)

;;(add-to-list 'package-archives '("elpa-devel" . "https://elpa.gnu.org/devel/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
;;(add-to-list 'package-archives '("nongnu-devel" . "https://elpa.nongnu.org/nongnu-devel/"))
;;(add-to-list 'package-archives '("ELPA" . "https://tromey.com/elpa/"))

;; Switch to Mirrors
;; https://github.com/melpa/melpa#mirrors
;;(setf (cdr (assoc "melpa"        package-archives)) "https://www.mirrorservice.org/sites/melpa.org/packages/")
;;(setf (cdr (assoc "melpa-stable" package-archives)) "https://www.mirrorservice.org/sites/stable.melpa.org/packages/")

;; アーカイブの優先順位 (数値が大きい方が優先; it works?)
(customize-set-variable
 'package-archive-priorities
 '(("manual" . 99)
   ("gnu" . 40)
   ("nongnu" . 30)
   ("melpa-stable" . 20)
   ("melpa" . 10)
   ))

;;(package-initialize)

(with-eval-after-load "package"
  (setopt package-native-compile t)
  (define-key package-menu-mode-map (kbd "I") 'package-install)
  (define-key package-menu-mode-map (kbd "D") 'package-uninstall)
  (define-key package-menu-mode-map (kbd "?") 'describe-package)
  (define-key package-menu-mode-map (kbd "/") 'occur))

(defun user::package-uninstall (pkg)
  "Uninstall the package PKG."
  (interactive (list (intern (completing-read "Uninstall package: " package-alist))))
  (pcase-let ((`(,_name ,desc)
               (assq pkg package-alist)))
    (if (package-desc-p desc)
        (package-delete desc))))

(unless (symbol-function 'package-uninstall)
  (setf (symbol-function 'package-uninstall) #'user::package-uninstall))

;(setopt package-native-compile t)

;; TODO:
;; - try https://github.com/raxod502/straight.el
;; - :config (というかトップレベル以外) で補助関数を定義するとソースジャンプができない

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(use-package use-package
  :custom
  ;; `--debug-init' フラグ有効時にのみデバッグ出力
  (use-package-verbose (if init-file-debug t nil))
  (use-package-expand-minimally (if init-file-debug nil t))
  (use-package-enable-imenu-support t)  ; imenu -> Packages
  (use-package-ignore-unknown-keywords t)
  (use-package-compute-statistics t)    ; M-x `use-package-report'
  ;;(use-package-hook-name-suffix nil)  ; TODO: disable abbrev
  ;; 初回起動時は自動インストールしたい (でも CI ではスキップしたい)
  (use-package-always-ensure
   (if (or (getenv "TRAVIS")
           (getenv "GITHUB_ACTIONS"))
       nil t))
  ;; 遅延ロード
  (use-package-always-defer t)
  :config
  (defface use-package--disabled-keyword-face
    '((t (:inverse-video t)))
    ":disabled キーワードを強調する face.")
  (font-lock-add-keywords 'emacs-lisp-mode
    '(("\\_<:disabled\\_>" . 'use-package--disabled-keyword-face)))
  )

;; FIXME: `use-package-always-ensure' 有効時に built-in ライブラリを
;; インストールしようとする挙動が解消されればこのマクロは不要
(defmacro use-package* (name &rest args)
  "Similar to `use-package', but does not require NAME to be installed.
It uses for built-in package configuration."
  `(use-package ,name
     :ensure nil
     ,@args))

;; modernizing Emacs Package Menu
;; NOTE: Archived since Oct 22, 2022
(use-package paradox
  :disabled
  :pin #:melpa-stable
  :demand
  :config (paradox-enable)
  :custom
  (paradox-execute-asynchronously t)
  ;; "⛺" を使いたいが `tabulated-list' は今のところ emoji 非対応らしい
  (paradox-homepage-button-string "⌂")
  (paradox-column-width-version 13)     ; melpa 20190123.4567 (13 characters)
  )

;; パッケージ管理ソフトいろいろ
;; package.el
;; straight.el	- https://github.com/radian-software/straight.el
;; quelpa	- https://github.com/quelpa/quelpa
;; elpaca	- https://github.com/progfolio/elpaca
;; Borg		- https://github.com/emacscollective/borg
