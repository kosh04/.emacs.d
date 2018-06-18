;;; config/dired.el

(require 'dired)
(require 'dired-x)
(require 'dired-aux)

(setq dired-dwim-target t)                ; 二窓用
(setq dired-isearch-filenames 'dwim)      ; ファイル名だけをisearch
;;(setq dired-recursive-deletes 'always)
(setq dired-listing-switches "-alh")

(add-hook 'dired-mode-hook 'hl-line-mode) ; 行カーソル

;; ファイル名のみ表示するマイナーモード (since 24.4)
(declare-function 'dired-hide-details-mode "dired")

;; dired-x で代用可能
;; [C-x C-j]   dired-jump
;; [C-x 4 C-j] dired-jump-other-window
(defadvice dired (around set-forcus-on-filename activate)
  "作業中のファイルにカーソルを当てる."
  (let ((file (buffer-file-name)))
    ad-do-it
    (if file (dired-goto-file file))))

;; [v] dired-view-file
(defun user/read-only-mode (&rest r)
  "読み取り専用でファイルを開く."
  (read-only-mode))
(advice-add 'dired-find-file              :after 'user/read-only-mode)
(advice-add 'dired-find-file-other-window :after 'user/read-only-mode)

(defun dired-copy-pathname-as-kill ()
  "ファイルのフルパスを取得."
  (interactive)
  ;; With a zero prefix arg, use the absolute file name of each marked file.
  (dired-copy-filename-as-kill 0))

;; ファイル一覧の最上部/最下部に移動するコマンドはデフォルトでないの？
(defun user:dired-beginning-of-buffer ()
  "Jump to the first listed in Dired."
  (interactive)
  (goto-char (point-min))
  (dired-next-line 2))

(defun user:dired-end-of-buffer ()
  "Jump to the last file listed in Dired."
  (interactive)
  (goto-char (point-max))
  (dired-next-line -1))

;; (defun dired-shell-execute ()
;;   "ファイルを関連付けられたプログラムで開く."
;;   (interactive)
;;   (let ((file (dired-get-filename)))
;;     (and file
;;          (or (file-exists-p file)
;;              (error "Not found %s" file))
;;          (y-or-n-p (format "Execute %s? " (file-namestring file)))
;;          ;; Linux:gnome-open(xdg-open) / Win32:start
;;          (shell-execute file))))

(require 'ls-lisp)
(setq ls-lisp-use-insert-directory-program nil)
(setq ls-lisp-dirs-first t)
;; (setq dired-listing-switches "-al --group-directories-first")
;; (setq dired-listing-switches "-al")
;; (setq ls-lisp-emulation t ls-lisp-ignore-case t)

;; [(dired) a]
;; dired バッファを削除した後で find-file (のキーバインドを有効にする)
(put 'dired-find-alternate-file 'disabled nil)

;; いろいろなソート
;; http://www.emacswiki.org/emacs/dired-sort.el
(defmacro define-dired-sort-by (name switch)
  `(defun ,(intern (format "dired-sort-by-%s" name)) ()
     (interactive)
     (dired-sort-other (concat dired-listing-switches ,switch))))

(define-dired-sort-by size "S")         ; ファイルサイズ
(define-dired-sort-by extension "X")    ; 拡張子
(define-dired-sort-by mtime "t")        ; タイムスタンプ
(define-dired-sort-by ctime "c")        ; 修正時刻
(define-dired-sort-by atime "u")        ; 最終アクセス時刻
(define-dired-sort-by version "v")      ; バージョン名／番号
(define-dired-sort-by reverse "r")      ; 逆順
(define-dired-sort-by name "")

(with-eval-after-load "dired"
  ;;(define-key dired-mode-map "W" 'dired-copy-pathname-as-kill)
  ;;(define-key dired-mode-map "q" 'kill-this-buffer) ; use C-u q
  ;;(define-key dired-mode-map "X" 'dired-shell-execute)
  (define-key dired-mode-map (kbd "C-x C-q") 'wdired-change-to-wdired-mode)
  (define-key dired-mode-map [remap beginning-of-buffer] 'user:dired-beginning-of-buffer)
  (define-key dired-mode-map [remap backward-page] 'user:dired-beginning-of-buffer)
  (define-key dired-mode-map [remap end-of-buffer] 'user:dired-end-of-buffer)
  (define-key dired-mode-map [remap forward-page] 'user:dired-end-of-buffer)
  ;; (define-key dired-mode-map "e" #'(lambda () "Open file via EWW." (interactive) (eww-open-file (dired-get-file-for-visit))))
  t)

(with-eval-after-load "wdired"
  (custom-set-variables
   '(wdired-allow-to-change-permissions t))
  (define-key wdired-mode-map (kbd "C-x C-q") 'wdired-finish-edit))

;; ファイル表示をマスク
;;(add-hook 'dired-mode-hook 'dired-omit-mode)

(custom-set-variables
 ;; 外部コマンドの関連付け
 `(dired-guess-shell-alist-user
   '((,(rx "." (or "mp3" "ogg" "wav" "aac") eos)
      "ffplay"))))

;; interactive filter
(use-package dired-narrow
  :defer t
  :bind (:map dired-mode-map ("/" . dired-narrow)))

;; ファイルの中身を覗き見る (peep)
(use-package peep-dired
  :defer t
  :config
  (custom-set-variables
   ;;'(peep-dired-cleanup-eagerly t)
   )
  :bind (:map dired-mode-map
              ("W" . peep-dired)
         :map peep-dired-mode-map
              ("n" . peep-dired-next-file)
              ("p" . peep-dired-prev-file)
              ("<SPC>" . peep-dired-scroll-page-down)
              ("<S-SPC>" . peep-dired-scroll-page-up)))

;; ファイルに関連付けられたアイコンを表示する
;; darwin: ~/Library/Fonts/*.ttf
(use-package all-the-icons-dired
  ;; NTEmacsだと描画が重いため見送り (追加で font-lock+.el が必要)
  :if (eq system-type 'darwin)
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
  (diminish 'all-the-icons-dired-mode " "))
