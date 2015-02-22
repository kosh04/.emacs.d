;;; memo/dired.el

(require 'wdired)
;; diredバッファでファイルを編集
;; dired-toggle-read-only [C-x C-q] でも可
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)
(setf (lookup-key wdired-mode-map "\C-c\C-q") 'wdired-finish-edit)
(define-key dired-mode-map [f2] 'dired-do-rename)
(call-interactively #'image-dired)      ; 画像一覧
;; ls-lisp.el
(setq ls-lisp-use-insert-directory-program nil)

(dired-flag-garbage-files)              ; [&] (いらないファイルをマーク)
(dired-flag-auto-save-files)            ; [#] (自動セーブファイルをマーク)

;; よく使う実行コマンド :dired-x
dired-guess-shell-alist-default

;; dired からファイルの中身をちょこっと見たい時に
;; 文字コード・改行文字の判別がイマイチ
(defun dired-head ()
  (interactive)
  (shell-command (format "/usr/bin/head \"%s\"" (dired-get-filename))
                 (get-buffer-create " *head*")))

;; 最近更新されたファイルを強調表示
(defface dired-new-modified '((t (:inherit font-lock-keyword-face)))
  "修正のあるファイルの色")

(defface dired-new-modified '((t (:foreground "sea green" :bold t)))
  "修正のあるファイルの色")
(defvar dired-new-modified-face 'dired-new-modified)
(defun dired-search-today (arg)
  (let ((system-time-locale "C"))
    (search-forward-regexp (format-time-string
                            "%m-%d [0-9]\\{2\\}:[0-9]\\{2\\}")
                           arg t)))
(font-lock-add-keywords 'dired-mode
  '((dired-search-today . dired-new-modified-face)))

(defface! dired-new-modified
    '((t (:foreground "black" :bold t)))
  "修正のあるファイルの色")

(defun dired-copy-pathname-as-kill (&optional localp)
  "カーソル直下のファイルのフルパス名をコピーする."
  (interactive "P")
  (let ((filename (dired-get-filename localp)))
    (kill-new filename)
    (message "%s" filename)))
;;-> (dired-copy-filename-as-kill 0) と同じ

;; ブラウザで開く
(fset 'dired-browse-url-at-point #'browse-url-of-dired-file)
