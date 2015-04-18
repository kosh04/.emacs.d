;;; config/dired.el

(require 'dired)
(require 'dired-x)
(require 'dired-aux)

(setq dired-dwim-target t)                ; 二窓用
(setq dired-isearch-filenames t)
;;(setq dired-recursive-deletes 'always)

(add-hook 'dired-mode-hook 'hl-line-mode) ; 行カーソル

;; dired-x で代用可能
;; [C-x C-j]   dired-jump
;; [C-x 4 C-j] dired-jump-other-window
(defadvice dired (around set-forcus-on-filename activate)
  "作業中のファイルにカーソルを当てる."
  (let ((file (buffer-file-name)))
    ad-do-it
    (and file (dired-goto-file file))))

;; [v] dired-view-file
(defadvice dired-find-file (after read-only-mode activate)
  "読み込み専用でファイルを開く."
  (read-only-mode))

(defun dired-copy-pathname-as-kill ()
  "ファイルのフルパスを取得."
  (interactive)
  ;; With a zero prefix arg, use the absolute file name of each marked file.
  (dired-copy-filename-as-kill 0))

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

(with-eval-after-load "dired"
  (define-key dired-mode-map "W" 'dired-copy-pathname-as-kill)
  (define-key dired-mode-map "q" 'kill-this-buffer)
  ;;(define-key dired-mode-map "X" 'dired-shell-execute)
  (define-key dired-mode-map (kbd "C-s") 'dired-isearch-filenames)
  (define-key dired-mode-map (kbd "C-x C-q") 'wdired-change-to-wdired-mode)
  t)

(with-eval-after-load "wdired"
  (define-key wdired-mode-map (kbd "C-x C-q") 'wdired-finish-edit)
  t)
