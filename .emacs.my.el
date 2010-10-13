;;; -*- mode: emacs-lisp; coding:utf-8 -*-
;;;
;;; This file is NOT part of Emacs.

;;; Time-stamp: <2010-10-13T09:16:26JST>

;;; @@Debug

;; どのパッケージがCLをロードするのか
;; (eval-after-load "cl"
;;   (quote (message "Loading CL: %S" load-file-name)))

;; (eval-after-load "utf-8"
;;   (quote (progn
;;            (message "Loading UTF-8: %S" load-file-name))))

(unless (string-equal current-language-environment "Japanese")
  (set-language-environment "Japanese"))

;;; @@load-path

;; このファイルを基準とした以下のディレクトリをload-pathに追加する
(when load-file-name
  (let ((default-directory (file-name-directory load-file-name)))
    (add-to-list 'load-path default-directory)
    ;; (normal-top-level-add-subdirs-to-load-path)
    (load "./subdirs.el") ))

(add-to-list 'load-path "~/.emacs.d/")

;; (require 'cl)
(when (locate-library "xyzzy")
  (require 'xyzzy)
  (require 'xyzzy-keymap)
  (require 'xyzzy-util))
(global-set-key (kbd "C-x c") 'run-console)

(global-set-key (kbd "C-x C-c")
                (cond (window-system 'iconify-frame)
                      (:else 'suspend-emacs)))
;; (define-key help-map [(control ?x) (control ?c)] 'kill-emacs)

;; Help [M-?, F1]
(or (lookup-key global-map "\M-?")
    (global-set-key "\M-?" help-map))

;;; Backup
(setq make-backup-files t)
(setq backup-by-copying t)
;(setq version-control t)
;; (add-to-list 'backup-directory-alist '("." . "~/.backup"))
;(setq backup-directory-alist `(("." . ,temporary-file-directory)))
;; (setq make-backup-file-name-function #'(lambda (file) (expand-file-name file "/tmp")))

;; flet, labels のインデント修正方法 (よろしくないかも)
(fset 'lisp-indent-function #'common-lisp-indent-function)

(add-to-list 'auto-mode-alist '("\\.elc?$" . emacs-lisp-mode))

;; -> xyzzy-keymap.el
;; (global-set-key "\C-h" 'delete-backward-char)
;; (define-key isearch-mode-map "\C-h" 'isearch-delete-char)

;;; highlight-line
(require 'hl-line)
(add-hook 'dired-mode-hook 'hl-line-mode)
(add-hook 'tar-mode-hook 'hl-line-mode)
;; (add-hook 'buffer-menu-mode-hook 'hl-line-mode)

;;; auto-fill
(setq default-fill-column 74)
(add-hook 'latex-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'html-mode-hook 'turn-off-auto-fill)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 表示関係
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; スクロールバーは右側
(when window-system
  (set-scroll-bar-mode 'right)
  (tool-bar-mode -1))
(setq visible-bell nil)
(transient-mark-mode t)
(tooltip-mode 0)
(global-font-lock-mode t)
;; (setq inhibit-startup-screen t)     ; スタートアップ画面を表示しない
;; (setq initial-scratch-message "")    ; *scratch* バッファに入る文字列

;; Buffer size
(size-indication-mode 1)

;;; modeline
(when window-system
  (set-face-background 'modeline "black")
  (set-face-foreground 'modeline "grey90"))
(line-number-mode t)
(column-number-mode t)

;;; [UN*X.emacs] から
;;(c-add-style "mystyle" '((c-basic-offset . 4)))
(defun my-indent-style ()
  (c-set-style "gnu")
  ; (setq c-basic-offset 2)
  )
(add-hook 'c-mode-common-hook   'my-indent-style)
(add-hook 'c++-mode-common-hook 'my-indent-style)

;;; frame
(setq frame-title-format
      `(" %b " (buffer-file-name "(%f)") " on " ,(system-name)
               " - " ,(if (featurep 'meadow)
                          (Meadow-version)
                          (format "Emacs %s" emacs-version))))

;;; paren
(show-paren-mode t)			; 対応する括弧に色をつける
;; (setq show-paren-style 'expression) ; カッコ全体をハイライト
(setq parse-sexp-ignore-comments t)	; コメント内のカッコは無視。

(put 'font-lock-add-keywords 'lisp-indent-function 1)

;;; タブ、全角スペースを可視化
;; (require 'my-font-lock-mode)

(load "hi-lock-x.el")
;; (global-hi-lock-mode)
(setq hi-lock-file-patterns-policy nil)

;;; Elisp をさらに色付け
;; (require 'elisp-font-lock)
;; 再帰が深すぎるとスタックが足りなくなるので適当に大きく
(setq max-lisp-eval-depth 1000)
;; (setq max-specpdl-size 3000)
;; elisp-font-lock.el 組み込み関数の数は固定だよね
(defvar elisp-subr-keywords
  (let (acc)
    (mapatoms #'(lambda (x)
                  (if (subrp (indirect-function x t))
                      (push (symbol-name x) acc))))
    (regexp-opt (nreverse acc) 'words)))
(mapc #'(lambda (mode)
          (font-lock-add-keywords mode
            `((,elisp-subr-keywords . font-lock-keyword-face))))
      '(lisp-interaction-mode emacs-lisp-mode))

;;; バッファのタブ化
(load "tabbar" t)
;;(load "c:/home/tabbar.el" t)
(eval-after-load "tabbar"
  (quote
   (progn
     (tabbar-mode 1)
     ;; kill-bufferしたときに戻るバッファが変わるのを抑える
     ;; http://www.bookshelf.jp/pukiwiki/pukiwiki.php?Elisp%2F%A5%BF%A5%D6%A4%C7%A5%D0%A5%C3%A5%D5%A5%A1%A4%F2%C0%DA%A4%EA%C2%D8%A4%A8%A4%EB
     (remove-hook 'kill-buffer-hook 'tabbar-buffer-kill-buffer-hook)
     ;; グループ化しない
     (setq tabbar-buffer-groups-function #'(lambda (buffer) (list "All Buffers"))
           tabbar-buffer-list-function
           #'(lambda ()
               (delq nil
                     (mapcar (lambda (buffer)
                               (and (or (string-match "^[^ *]" (buffer-name buffer))
                                        (member (buffer-name buffer)
                                                '("*scratch*" "*shell*"
                                                  "*Diff*"
                                                  "*ruby*" "*newlisp*")))
                                    buffer))
                             (buffer-list)))))

     ;; 外観の変更
     (set-face-attribute 'tabbar-default-face nil :background "gray60")
     (set-face-attribute 'tabbar-unselected-face nil :background "gray85" :foreground "gray30" :box nil)
     (set-face-attribute 'tabbar-selected-face nil :background "#f2f2f6" :foreground "black" :box nil)
     (set-face-attribute 'tabbar-button-face nil :box '(:line-width 1 :color "gray72" :style released-button))
     (set-face-attribute 'tabbar-separator-face nil :height 0.7)
     ;; gnome-terminalだとC-. C-, が効かないの忘れてた
     (global-set-key (kbd "C-x C-.")  'tabbar-forward-tab)
     (global-set-key (kbd "C-x C-,") 'tabbar-backward-tab)

     ;; 中央ボタンでバッファのキル
     (defun tabbar-buffer-select-tab (event tab)
       "On mouse EVENT, select TAB."
       (let ((buffer (tabbar-tab-value tab)))
         (case (event-basic-type event)
           (mouse-1 (switch-to-buffer buffer))
           (mouse-2 (kill-buffer buffer)) ; (pop-to-buffer buffer t)
           (mouse-3 (delete-other-windows)))
         ;; Disable group mode.
         (setq tabbar-buffer-group-mode nil)
         ))
     (setq tabbar-select-tab-function #'tabbar-buffer-select-tab)
     ;; (defun tabbar-buffer-help-on-tab () )
     ;; (defun tabbar-buffer-toggle-group-mode-help () "")

     ;; mouse-avoidance-mode と被ると面倒なので無視する
     (setq tabbar-help-on-tab-function 'ignore)
     (setq tabbar-home-help-function 'ignore)
     )))


;; 補完可能なものを随時表示 (少しうるさい)
;; FIXME: 入力だけでなく移動でも補完されるのがうざったいので何とかしたい
(icomplete-mode t)

;; 全て差し替えるかどうか、それが問題だ
(defmacro eval-after (file &rest args)
  `(eval-after-load ,file
     (quote (progn ,@args))))
(put 'eval-after 'lisp-indent-function 1)

(eval-after "newcomment"
  (setq comment-style 'multi-line))

;;; shell-command(M-!) のコマンド入力に補完を効かせる
;;; http://namazu.org/~tsuchiya/
(load "shell-command" t)
(eval-after "shell-command"
  (shell-command-completion-mode t))

;;; エスケープシーケンスを処理する ("ls --color" が使える)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(defun shell-other-window ()
  (interactive)
  (let ((buffer (save-window-excursion
                  ;; Enable current-prefix-arg
                  (call-interactively 'shell))))
    (or (eq buffer (current-buffer))
        (switch-to-buffer-other-window buffer))))

(global-set-key (kbd "C-c s") 'shell-other-window)

;;; URLをブラウザで開く
;; (global-set-key [S-mouse-2] 'browse-url-at-mouse)
(eval-after-load "browse-url"
  (quote
    (progn
     (global-set-key "\C-c\C-o" 'browse-url-at-point)
     (setq browse-url-browser-function 'browse-url-generic
           browse-url-generic-program
           ;; 端末からの利用ならw3mてのもありかも
           (cond ((eq system-type 'windows-nt)
                  "c:/Program Files/Mozilla Firefox/firefox.exe") ; or "start"
                 ((eq system-type 'gnu/linux)
                  "x-www-browser"))
           ;; browse-url-generic-args '("-a" "Safari")
           )
     )))

;;; バッファの末尾以降の空白が見える (fringe)
;; (set-fringe-mode 5)
(setq-default indicate-empty-lines t)
(set-face-background 'fringe "gray80")

;;; 日付と時刻の挿入
;;; http://www.bookshelf.jp/texi/elisp-manual-20-2.5-jp/elisp_38.html#SEC610
(defun insert-time ()
  (interactive)
  (let ((system-time-locale "C"))
    (insert (format-time-string "%Y-%m-%dT%H:%M:%S"))))
(global-set-key "\C-ct" 'insert-time)

;;; gzファイルも編集できるように
(auto-compression-mode t)

;; インデントにtabを使うかどうか
(setq-default indent-tabs-mode nil)
;; (setq-default indent-tabs-mode t)

;;; Emacs Lisp 関連

;;; Eldoc は便利です
;; ** Emacs23のeldocは対応する仮引数がハイライトされるようになっている。
;; http://d.hatena.ne.jp/rubikitch/20090207/1233936430
;; (load-file "c:/home/emacs/22.1/lisp/emacs-lisp/eldoc.el")
;; (load "_eldoc" t)
;; (require '_eldoc)                       ; emacs23用のソース
(autoload 'turn-on-eldoc-mode "eldoc" nil t)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)

;; (load "eldoc-extension" t)
;; SLIME風arglist
;; (set-face-background 'eldoc-highlight-function-argument "darkseagreen2")
;; (set-face-bold-p 'eldoc-highlight-function-argument nil)
;; (setq eldoc-idle-delay 0.2 eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit)
;; (global-set-key "\C-xtl" 'eldoc-mode)

;; imenu
(add-hook 'emacs-lisp-mode-hook #'imenu-add-menubar-index)

;;; カーソルあっちいって
;; (display-mouse-p)
(if (eq system-type 'windows-nt)
    (mouse-avoidance-mode 'exile)
    (mouse-avoidance-mode 'banish))

;;; カーソル上にあるファイル名や URL を開く
;;; フツーの Find file: は C-u C-x C-f
;; (require 'ffap)
;; (ffap-bindings)

(defun ffap-emacs ()
  "URLをemacsのバッファに開く."
  (interactive)
  (url-handler-mode t)
  (let ((ffap-url-fetcher #'find-file))
    (call-interactively #'ffap)))

;;; 関数一覧 (M-x: imenu)
(require 'imenu)
(global-set-key (kbd "C-c C-l") 'imenu)

;;; インデントして次の行へ
(defun indent-and-next-line (&optional args)
  (interactive "p")
  (dotimes (x args)
    (indent-according-to-mode)
    (line-move 1)))
;; (byte-compile 'indent-and-next-line)
(global-set-key "\M-n" 'indent-and-next-line)

;;; 物理行移動マイナーモード
;;; http://xem.jp/~tkng/physical-line.el ; 404 Not Found
;;; http://taiyaki.org/elisp/physical-line/ ; C-e がうまく動かないなぁ
;;; http://www.scythe.jp/lab/physical-line.html ; これが今のとこイイ
(load "physical-line" t)
(eval-after "physical-line"
  (setq-default physical-line-mode t))

;;; EOF は必ず改行で
(setq require-final-newline t)

;;; @@Woman
(eval-after-load "woman"
  (quote
   (progn
     (add-to-list 'woman-manpath "/usr/share/man/ja/")
     (setq woman-use-own-frame nil)
     (setq woman-cache-filename "~/.emacs.d/woman-cache.el")
     )))

;; 大文字小文字の区別をしない
(setq completion-ignore-case t
      read-file-name-completion-ignore-case t)
(if (boundp 'read-buffer-completion-ignore-case) ; Emacs23.1
    (setq read-buffer-completion-ignore-case t))

;;; 誤爆するとイヤなので無効化
(global-unset-key "\C-xm")		; compose-mail
(global-unset-key "\C-x\C-n")		; set-goal-column

;;; Google 検索
(require 'google)
(global-set-key "\C-cg" 'google-search)

;; FIXME: w3mからは参照できない
(defun google-translate (string)
  "Google 翻訳"
  (interactive "sText: ")
  (browse-url (concat
               "http://translate.google.com/translate_t?hl=ja"
               (if (multibyte-string-p string) "#en|ja|" "#ja|en|")
               (decode-coding-string string 'utf-8))))

;;; @@Desktop -- 終了時の状態を保存
(desktop-save-mode t)
(setq desktop-load-locked-desktop nil)
;; (fset 'restore-desktop #'desktop-read)
(defun restore-desktop (&optional filename)
  (interactive (list (read-file-name "Desktop file: " "~/.emacs.desktop")))
  (let ((desktop-first-buffer nil)
        (desktop-buffer-ok-count 0)
        (desktop-buffer-fail-count 0))
    (load filename 'noerror 'nomsg 'nosfx))
  ;; (desktop-read (directory-namestring filename))
  )

;; 内部変数を保存しておく
(require 'session)
(eval-after-load "session"
  (quote
   (progn
     (add-hook 'after-init-hook #'session-initialize))))

;;; スクリプトを保存する時，自動的に chmod +x を行う
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;;; inferior-lisp
;; "c:/usr/local/clisp-2.47-full/bin/clisp.exe"
;; "c:/cygwin/bin/clisp.exe"
(if (eq system-type 'windows-nt)
    (setq inferior-lisp-program "C:/usr/local/clisp-2.44/clisp.exe"))

;; (autoload 'lisp-eval-last-sexp "inf-lisp")
;; (global-set-key "\C-c\C-x" 'lisp-eval-last-sexp)
;; (autoload 'switch-to-lisp "inf-lisp" nil t)
;; (global-set-key "\C-cj" 'switch-to-lisp)

(add-to-list 'auto-mode-alist '("\\.cl$" . lisp-mode))

;;; @@SLIME
(when (eq system-type 'windows-nt)      ; (locate-library "clisp-olio")
  (require 'clisp-olio)
  ;; (define-key slime-repl-mode-map "\C-ch" 'hyperspec-lookup)
  (defun run-gcl (&optional ansi)
    (interactive "P")
    (run-lisp (if ansi
                  "C:/PROGRA~1/GCL-26~1.7-A/lib/GCL-26~1.7/unixport/SAVED_~1.EXE"
                  "C:/PROGRA~1/GCL-26~1.7-C/lib/GCL-26~1.7/unixport/SAVED_~1.EXE")))
  )

(eval-after-load "slime"
  (quote
   (progn
     (defadvice slime-eval-last-expression (before eval-safe activate)
       (skip-syntax-forward "w_"))
     (defadvice slime-compute-autodoc (around check--interactive activate)
       ;; プロセスがない場合はメッセージを抑制する
       (if (slime-current-connection)
           ad-do-it))
     (fset 'switch-to-slime 'slime-repl)
     (fset 'slime-eval-expression 'slime-interactive-eval)
     (define-key slime-mode-map (kbd "M-:") 'slime-eval-expression)
     ;; スペースキーでちゃんと変換して
     (when (featurep 'anthy)
       (defadvice slime-space (around enable-anty-insert activate)
         ;; FIXME
         (if (and anthy-minor-mode
                  (not (string-equal anthy-preedit "")))
             (anthy-insert)
             ad-do-it)))
     (define-key emacs-lisp-mode-map (kbd "C-c C-d h") 'hyperspec-lookup)
     (define-key lisp-interaction-mode-map (kbd "C-c C-d h") 'hyperspec-lookup)
     )))

(eval-after-load "slime-fuzzy"
  (quote
   (progn
     (defadvice slime-fuzzy-done (around ignore-killed-buffer activate)
       (if (buffer-live-p slime-fuzzy-target-buffer)
           ad-do-it))
     (add-hook 'slime-net-process-close-hooks
               (defun slime-kill-misc-buffers (process)
                 (declare (ignore process))
                 (mapc (lambda (buffer)
                         (kill-buffer (get-buffer-create buffer)))
                       '("*Fuzzy Completions*"
                         "*compiler notes*"))))
     )))

;;; newLISP
;; (load-library "newlisp/newlisp")
(load "newlisp" t)
(eval-after-load "newlisp"
  (quote
   (progn
     (setq newlisp-switches "-C -s10000 -strict -v") ; -v は独自オプション
     (setq newlisp-manual-text
           (locate-file "newlisp_manual.txt" load-path))
     (add-to-list 'auto-mode-alist '("\\.lsp$" . newlisp-mode))
     (add-to-list 'interpreter-mode-alist '("newlisp" . newlisp-mode))
     (add-hook 'newlisp-mode-hook
               #'(lambda ()
                   (setq comment-start "; ")
                   ;; あまり好まないがタブ幅4のファイルがあまりに多いので
                   (setq tab-width 4)
                   ))
     (defun newlisp-browse-manual-w3m ()
       (interactive)
       (w3m-find-file newlisp-manual-html))
     (defun run-newlisp-sjis ()
       (interactive)
       (let ((newlisp-command "newlisp_sjis")
             (newlisp-process-coding-system 'sjis)
             (default-process-coding-system 'sjis))
         (run-newlisp)))
     (define-key newlisp-mode-map "\C-ch" 'newlisp-lookup-manual)
     )))

;;; swank-newlisp
(defun swank-newlisp-init (port-filename coding-system)
  (format "%S\n" `(swank:start-server ,port-filename)))

;; (setq slime-protocol-version nil)     ; (slime-changelog-date)
(defun slime-newlisp ()
  (interactive)
  (let ((slime-lisp-implementations
         `((newlisp ("newlisp" "-n" ,(locate-file "swank-newlisp.lsp" load-path))
                    :init swank-newlisp-init
                    :coding-system utf-8-unix
                    )) ))
    (slime 'newlisp)))

;; (global-set-key [?\C-8] 'insert-parentheses)
(global-set-key (kbd "M-\"")
                (defun insert-double-quotes (&optional arg)
                  (interactive "P")
                  (insert-pair arg ?\" ?\")))
(global-set-key (kbd "M-#")
                (defun insert-multi-comment-for-lisp (&optional arg)
                  (interactive "P")
                  (insert "#|" "|#")
                  (backward-char 2)))
;; (global-set-key [?\(] 'insert-parentheses)
;; (global-set-key [?\"] 'insert-pair)
(global-set-key "\M-'" 'insert-pair)    ; (kbd "ESC '")
(global-set-key (kbd "ESC `") 'insert-pair) ; "M-`"

;; etags は標準で再帰ができない様子
;; 代わりにEmacs標準関数を使おうか
;; #'find-library #'find-variable #'find-function
(setq find-function-C-source-directory
      (if (eq system-type 'windows-nt)
          "~/src/emacs-22.2/src"
          "~/src/emacs-22/src/"))
;; (global-set-key [(control ?c) ?f] 'find-library)
(define-key help-map (kbd "C-l") 'find-library)
;; (define-key ctl-x-map [?j] 'find-function)
(define-key help-map "j" 'find-function)

(defun find-symbol-at-point ()
  (interactive)
  ;; (or (find-variable-at-point) (find-function-at-point))
  (let ((sym (symbol-at-point)))
    (if (memq major-mode '(emacs-lisp-mode lisp-interaction-mode))
        ;; elisp mode
        (cond
          ((null sym) nil)
          ((boundp sym) (find-variable-other-window sym))
          ((fboundp sym) (find-function-other-window sym))
          (t
           (error "not found: %s" sym)
           ;; (call-interactively 'find-tag)
           ))
        (progn
          ;; other program
          (visit-tags-table (expand-file-name "TAGS") t) ; "local"必要？
          (find-tag (symbol-name sym))))))
(define-key esc-map [?.] 'find-symbol-at-point)

;;; Buffer List 関係
;; (load-library "buff-menu")
;; (define-key Buffer-menu-mode-map [(control ?g)] 'quit-window)
;; (add-hook 'buffer-menu-mode-hook #'(lambda () (pop-to-buffer "*Buffer List*")))
;; (global-set-key [(control ?x) (control ?b)] 'electric-buffer-list)
;; (setq Buffer-menu-sort-column 2)        ; '(CRM Buffer Size Mode File)
;; (eval-after-load 'ebuff-menu
;;   (quote (progn
;;            (define-key electric-buffer-menu-mode-map "\C-g" 'Electric-buffer-menu-quit))))
;; (global-set-key "\C-x\C-b" 'buffer-menu-other-window)
;; (define-key Buffer-menu-mode-map "\C-g" 'kill-buffer-and-window)
(global-set-key (kbd "C-x C-b") 'bs-show)
(add-hook 'bs-mode-hook 'hl-line-mode)
;; (setq Buffer-menu-sort-column 2)        ; '(CRM Buffer Size Mode File)

(defun sequence (from to &optional step)
  (if (<= from to)
      (number-sequence from to step)
      (nreverse (number-sequence to from step))))

;;; @@Dired
(require 'ls-lisp)
(setq ls-lisp-use-insert-directory-program nil)
(setq ls-lisp-dirs-first t)
;; (setq dired-listing-switches "-al --group-directories-first")
;; (setq dired-listing-switches "-al")
;; (setq ls-lisp-emulation t ls-lisp-ignore-case t)

;; dired-other-window に使うなら、同じ形で別定義する必要あり
(defadvice dired (around set-forcus-on-filename activate)
  "作業中のファイルにカーソルを当てる."
  (let ((file (buffer-file-name)))
    ad-do-it
    (and file (dired-goto-file file))))

(defun dired-copy-pathname-as-kill ()
  "ファイルのフルパスを取得."
  (interactive)
  ;; With a zero prefix arg, use the absolute file name of each marked file.
  (dired-copy-filename-as-kill 0))

(defun dired-shell-execute ()
  "ファイルを関連付けられたプログラムで開く."
  (interactive)
  (let ((file (dired-get-filename)))
    (and file
         (or (file-exists-p file)
             (error "Not found %s" file))
         (y-or-n-p (format "Execute %s? " (file-namestring file)))
         ;; Linux:gnome-open(xdg-open) / Win32:start
         (shell-execute file))))

(eval-after-load "dired"
  (quote
   (progn
     (define-key dired-mode-map "W" 'dired-copy-pathname-as-kill)
     (define-key dired-mode-map "q" 'kill-this-buffer)
     (define-key dired-mode-map "X" 'dired-shell-execute)
     ;; wdired
     (define-key dired-mode-map "\C-x\C-q" 'wdired-change-to-wdired-mode)
     )))

(eval-after-load "wdired"
  (quote
   (progn
     (define-key wdired-mode-map "\C-x\C-q" 'wdired-finish-edit))))

;; [(dired) a]
;; diredバッファを消してからfind-file (のキーバインドを有効)
(put 'dired-find-alternate-file 'disabled nil)

;; いろいろなソート
;; http://www.emacswiki.org/emacs/dired-sort.el
(defmacro define-dired-sort-by (name switch)
  `(defun ,(intern (format "dired-sort-by-%s" name)) ()
     (interactive)
     (dired-sort-other (concat dired-listing-switches ,switch))))

;; NOTE: -r  ソートの順番を逆にする。
(define-dired-sort-by size "S")         ; ファイルサイズ
(define-dired-sort-by extension "X")    ; 拡張子
(define-dired-sort-by mtime "t")        ; タイムスタンプ
(define-dired-sort-by ctime "c")        ; 修正時刻
(define-dired-sort-by atime "u")        ; 最終アクセス時刻
(define-dired-sort-by version "v")      ; バージョン名／番号

(defadvice dired-find-file (after read-only-mode activate)
  "Diredからファイルを開く時は読み込み専用にする."
  (toggle-read-only 1))

;; ファイル名だけを探索するisearch
;; http://www.emacswiki.org/emacs/dired-isearch.el
;; http://emacswiki.wikiwikiweb.de/cgi-bin/wiki/download/dired-isearch.el
(autoload 'dired-isearch-forward "dired-isearch")
(autoload 'dired-isearch-backward "dired-isearch")
(eval-after-load "dired"
  (quote
   (progn
     (define-key dired-mode-map (kbd "C-s") 'dired-isearch-forward)
     (define-key dired-mode-map (kbd "C-r") 'dired-isearch-backward)
     )))
;; (define-key dired-mode-map (kbd "ESC C-s") 'dired-isearch-forward-regexp)
;; (define-key dired-mode-map (kbd "ESC C-r") 'dired-isearch-backward-regexp)


(defun indent-line-sexp ()
  "長い一行S式をそれなりにインデントします."
  (interactive)
  (save-excursion
    (save-restriction
      ;; (while (condition-case nil (backward-up-list) (error nil)))
      (narrow-to-region (point)
                        (save-excursion
                          (forward-sexp)
                          (point)))
      ;; (replace-string ") " ")\n")
      (perform-replace ") " ")\n" nil  nil nil)
      ))
  (indent-sexp))

;;; @@Time-stamp
(require 'time-stamp)
(add-hook 'before-save-hook 'time-stamp)
(setq time-stamp-active t)
;; ISO 8601 フォーマットにしたいんだが...
;; FIXME: %z,%Zが効かないバグ？
(setq time-stamp-format "%04Y-%02m-%02dT%02H:%02M:%02S%Z")

;; "\M-\;\C-u\C-x\C-e"
;; (eval-last-sexp (universal-argument))は動作後にC-uが残る？
(global-set-key "\M-e"
                #'(lambda ()
                    (interactive)
                    (indent-for-comment)
                    ;; (ignore-errors (kill-line))
                    (command-execute "\C-u\C-x\C-e")))
(global-set-key [(control ?c) (control ?r)] 'eval-region)

(defun recompile-and-load-file ()
  "*.lc があれば再コンパイルとロード."
  (let ((file (buffer-file-name)))
    (when (and file
               (member major-mode '(emacs-lisp-mode lisp-interaction-mode))
               (file-exists-p (byte-compile-dest-file file))
               (< emacs-major-version 23) ; 23でコンパイルしたファイルは22で使えないので
               (null (check-parens)))
      (byte-compile-file file (featurep (intern (pathname-name file)))))))
(add-hook 'after-save-hook 'recompile-and-load-file)
;; (remove-hook 'after-save-hook 'recompile-and-load-file)

(global-set-key "\M-g" 'goto-line)
(global-set-key (kbd "C-x t f") 'toggle-truncate-lines)
;; toggle-trace-on-error@xyzzy
(global-set-key (kbd "C-x t e") 'toggle-debug-on-error)

;;; toggle-scratch-buffer (で呼び出している pop-to-buffer?) と相性が悪いかも
(require 'linum)

;;; ローカル変数セクションを無視
(setq local-enable-local-variables nil)

;;; @@Info
;; (require 'info)
(eval-after-load "info"
  (quote
   (progn
     (add-to-list 'Info-additional-directory-list "~/share/info/")
     ;; (define-key mode-specific-map [?h] 'info-lookup-symbol)
     )))
  
;;; 存在するバッファのみ切り替え
(defadvice switch-to-buffer (before existing-buffer activate compile)
  "When interactive, switch to existing buffers only, unless given a prefix argument."
  (interactive (list (read-buffer "Switch to buffer: "
                                  (other-buffer)
                                  (null current-prefix-arg)))))

(defun detect-and-decode-string (string)
  (mapcar #'(lambda (encoding)
              (cons encoding (decode-coding-string string encoding)))
          (detect-coding-string string)))

(fset 'encode #'encode-coding-string)
(fset 'decode #'decode-coding-string)
(fset 'detect #'detect-coding-string)

(or (lookup-key global-map [f12]) (global-set-key [f12] 'emacs-lisp-mode))
(or (lookup-key global-map [C-f12]) (global-set-key [C-f12] 'lisp-interaction-mode))

(when (eq system-type 'windows-nt)
  ;; (require 'ispell)
  (setq ispell-program-name (expand-file-name "Aspell/bin/aspell.exe" (getenv "PROGRAMFILES"))))

;; @@Ruby
(autoload 'ruby-mode "ruby-mode" "Major mode for ruby scripts." t)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
(when (eq system-type 'windows-nt)
  (require 'inf-ruby)
  (require 'ruby-mode) )
(eval-after-load "ruby-mode"
  (quote
   (progn
     )))

(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process" t)
(eval-after-load "inf-ruby"
  (quote
   (progn
     ;; "sh -c irb --inf-ruby-mode" でも起動はするけどプロンプトが現れない...
     (setq ruby-program-name "ruby -S irb --inf-ruby-mode")

     (defun ruby-send-line (&optional arg)
       (interactive "p")
       (ruby-send-region (point-at-bol 1) (point-at-eol arg))
       ;; (switch-to-ruby 'eob)
       )
     ;; ruby-insert-end [C-c C-e] 上書き
     (define-key ruby-mode-map (kbd "C-c C-e") 'ruby-send-line)

     (defadvice ruby-send-region (after display-buffer-tail activate)
       (save-selected-window
         (switch-to-ruby 'eob)))

     (defadvice switch-to-ruby (around switch-to-ruby-safe activate)
       (condition-case nil
           ad-do-it
         (error (call-interactively #'run-ruby))))
     )))


;; "*Help*" は消さない方が吉
(defvar *temporary-buffer-list* '("*Completions*"))
;; Ctrl-]
(defadvice abort-recursive-edit (before with-kill-temp-buffers activate)
  (let ((kill-buffer-safe
         (lambda (buffer)
           (and (get-buffer buffer) (kill-buffer buffer)))))
    (mapc kill-buffer-safe *temporary-buffer-list*)))

;; (fset 'trim-tailing-whitespace #'delete-trailing-whitespace)
(defun trim-tailing-whitespace ()
  (interactive)
  (save-restriction
    (when mark-active
      (narrow-to-region (region-beginning)
                        (region-end)))
    (delete-trailing-whitespace)))

;; 行末尾の余計な空白が気になる人用
;; (setq-default show-trailing-whitespace t)
(setq-default show-trailing-whitespace nil)

;; デフォルトでは折り返しいらない
(setq default-truncate-lines t)         ; 水平分割
(setq truncate-partial-width-windows t) ; 垂直分割

;; なぜか utf-8 になっていたので
;; (set-language-environment "japanese") のせいかな?
'(when (eq system-type 'windows-nt)
  (setq default-process-coding-system
        ;; '(japanese-iso-8bit . japanese-iso-8bit)
        '(sjis-unix . sjis-unix)
        ))

;; @@Migemo
(load "migemo/migemo" t)
(eval-after-load "migemo"
  (quote
   (progn
     ;; (defadvice isearch-mode (before migemo-off activate)
     ;;   "初期状態は常にnil"
     ;;   (setq migemo-isearch-enable-p nil))
     (setq migemo-isearch-enable-p nil)
     (fset 'toggle-migemo #'migemo-toggle-isearch-enable)
     (define-key isearch-mode-map (kbd "C-t") 'migemo-isearch-toggle-migemo)
     )))
;; キーバインドを変更したかったがisearch-mode-hookに埋め込まれているので断念

(defun insert-zenkaku-space ()
  "全角空白を挿入."
  (interactive)
  (insert (japanese-zenkaku " ")))
(fset 'insert-full-width-space #'insert-zenkaku-space)

;; @@url-handler
;; URL-FILEも扱えるように
(url-handler-mode 1)
(eval-after-load "url-handlers"
  (quote
   (progn
     ;; (url-handler-mode t)
     (defun url-file-name-completion (url directory &optional predicate) url)
     (defun url-file-name-all-completions (file directory) nil)
     ;; (setq ffap-url-fetcher #'find-file)
     )))

(defun url-load (url &optional noerror nomsg nosuffix must-suffix)
  (interactive (list
                (read-string "URL: " (thing-at-point 'url))))
  ;; (with-temp-buffer (url-insert-file-contents url) (eval-buffer))
  (url-handler-mode t)
  (unless (file-exists-p url)
    (error "Not Found load file: %s" url))
  (let ((file (merge-pathnames (file-namestring url)
                               temporary-file-directory)))
    (copy-file url file "overwrite")
    (pushnew file *temporally-files* :test #'equal)
    (load file noerror nomsg nosuffix must-suffix)))
(defvar *temporally-files* nil)
(add-hook 'kill-emacs-hook
          (defun delete-temporally-files ()
            (mapc #'(lambda (file)
                      (if (file-exists-p file)
                          (delete-file file)))
                  *temporally-files*)))
;; (setf (get 'load 'url-file-handlers) 'url-load)

(defun elisp-install-from-url (url)
  (interactive "sElisp url: ")
  (url-handler-mode t)
  (let ((file (merge-pathnames (file-namestring url) "~/.emacs.d/")))
    ;; 一時的に font-lock を無効にできないものか...
    ;; (font-lock-function #'identity)
    (copy-file url file "overwrite")
    (byte-compile-file file "load")))

;; javascrpt-mode
(defun js2-mode-install ()
  (interactive)
  (when (featurep 'js2-mode) (error "js2-mode already loaded."))
  (elisp-install-from-url
   "http://js2-mode.googlecode.com/svn/trunk/js2-mode.el"))

;; (load "js2-mode" t)
;; (url-load "http://js2-mode.googlecode.com/svn/trunk/js2-mode.el")
(autoload 'js2-mode "js2-mode" "Major mode for editing JavaScript code." t)
(fset 'javascript-mode 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(eval-after-load "js2-mode"
  (quote
   (progn
     (setq js2-enter-indents-newline t) ;  改行時にインデント
     )))

;; @@EmacsLispの再定義は色々危険だということを忘れずに...
(load "redef" t)

;; (url-load "http://jblevins.org/projects/markdown-mode/markdown-mode.el")
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(fset 'markdown-buffer #'markdown)      ; markdown-preview
;; (add-hook 'markdown-mode-hook
;;           (defun markdown-user-setup ()
;;             (setq tab-width 4)
;;             ))

;; Eshell
(defun eshell-user-setup-hook ()
  (define-key eshell-mode-map "\C-a" 'eshell-bol))

(eval-after-load "eshell"
  (quote
   (progn
     (add-hook 'eshell-mode-hook 'eshell-user-setup-hook)
     (setq eshell-ask-to-save-last-dir nil)
     )))

;; html+-mode@xyzzを真似てみよう
;; ただし端末ではCtrlキー入力が使えない...
(eval-after-load "sgml-mode"
  (quote
   (progn
     ;(fset 'sgml-quote-region #'sgml-quote)
     ;(fset 'html-quote-region #'sgml-quote)
     (define-key sgml-mode-map (kbd "C-,") 'sgml-tag)
     (define-key sgml-mode-map (kbd "C-.") 'sgml-close-tag)
     )))

;; ２重置換に注意 > "&amp;lt;"
(defun sgml-quote-region (from to &optional unquotep)
  (interactive "*r\nP")
  (let ((case-fold-search t))
    (format-replace-strings '(("&" . "&amp;")
                              ("<" . "&lt;")
                              (">" . "&gt;")
                              ("\"" . "&quot;")
                              ("\u00A0" . "&nbsp;"))
                            unquotep from to)))

(defun sgml-unquote-region (from to)
  (interactive "*r")
  (sgml-quote-region from to t))

;(define-key html-mode-map (kbd "C-c C-q") 'sgml-quote-region)

(defun unhtml-region (start end)
  "HTMLタグを除去する."
  (interactive "*r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (while (re-search-forward "<[^>]*>" nil t)
        (replace-match "")))))

(defun unicode-unescape-region (start end)
  "指定した範囲のUnicodeエスケープ文字(\\uXXXX)をデコードする."
  (interactive "*r")
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    (while (re-search-forward "\\\\u\\([[:xdigit:]]\\{4\\}\\)" nil t)
      (replace-match (string (unicode-char
                              (string-to-number (match-string 1) 16)))
                     nil t))))

(defun unicode-escape-region (&optional start end)
  "指定した範囲の文字をUnicodeエスケープする."
  (interactive "*r")
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    (while (re-search-forward "." nil t)
      (replace-match (format "\\u%04x"
                             (char-unicode
                              (char (match-string 0) 0)))
                     nil t))))

(defadvice locate (around modify-buffer-name activate)
  (let ((locate-buffer-name
         (format "*Locate %s*" search-string)))
    ad-do-it))

;; (global-set-key (kbd "M-w") 'clipboard-kill-ring-save)

;; @@Mew
(eval-after-load "mew"
  (quote
   (progn
     (define-key mew-summary-mode-map "g" 'mew-status-update)
     )))

;; @@Java
;; (require 'cc-mode)
(eval-after-load "cc-mode"
  (quote
   (progn
     (defun simple-java-compile-and-go (&optional args)
       (interactive (list (if current-prefix-arg
                              (read-string "with args: ") "")))
       (compile (format "javac %s && java %s %s"
                        #1=(file-name-nondirectory (buffer-file-name))
                        (file-name-sans-extension #1#)
                        args)))
     (define-key java-mode-map (kbd "C-c C-c") 'simple-java-compile-and-go)
     )))

;; いつの間にかマニュアルが文字化けしていた (2009-11-09)
(if (eq system-type 'windows-nt)
    (defadvice man (around manpage-ja activate)
      (let ((locale-coding-system 'euc-jp-unix))
        ad-do-it)))

(recentf-mode 1)

;; Racket - http://racket-lang.org/
(add-to-list 'auto-mode-alist '("\\.rkt\\'" . scheme-mode))
(defun run-racket ()
  (interactive)
  (run-scheme (if (eq system-type 'windows-nt)
                  "c:/PROGRA~1/Racket/Racket.exe"
                  "~/plt/bin/racket")))

;;; Server mode
;; NOTE: Meadow use `gnuserv'
(if (featurep 'meadow)
    (server-start))

;;; Tramp
;;(require 'tramp)
(eval-after-load "treamp"
  (quote
   (progn
     ;; (setq tramp-default-method "ssh")
     )))

;;; LaTeX
(eval-after-load "tex-mode"
  (quote
   (progn
     (setq latex-run-command "platex -kanji=euc")
     (add-to-list 'tex-compile-commands '("platex %f" "%f" "%f.dvi"))
     (setq tex-dvi-view-command "xdvi")
     (add-to-list 'auto-coding-alist '("\\.tex$" . japanese-iso-8bit))
     (add-to-list 'completion-ignored-extensions ".aux")
     (add-to-list 'completion-ignored-extensions ".dvi")
     ;; (add-to-list 'completion-ignored-extensions ".log")
     )))

;; EmacsWiki: Backup Directory - http://www.emacswiki.org/emacs/BackupDirectory
(setq make-backup-file-name-function
      #'(lambda (file)
          (let ((dirname (format-time-string
                          "~/.emacs.d/backups/%Y-%m-%d")))
            (or (file-directory-p dirname)
                ;; mkdir -p DIRNAME
                (make-directory dirname t))
            (expand-file-name (file-name-nondirectory file) dirname))))

;; FIXME
;; 賢いタブデリートとかいらない
;; (global-set-key (kbd "C-h") 'backward-delete-char)

;;; .emacs.my.el ends here
