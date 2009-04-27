;;; -*- mode: emacs-lisp -*-
;;;
;;; This file is NOT part of Emacs.
;;;

;;; Time-stamp: <2009-04-23T17:52:11JST>

;;; TODO
;;; setq & setq-default

(eval-after-load "cl"
  (quote (message "Loading CL: %S" load-file-name)))

;; (require 'cl)
(when (locate-library "xyzzy")
  (require 'xyzzy)
  (require 'xyzzy-keymap)
  (require 'xyzzy-util))

;; flet, labels のインデント修正方法 (よろしくないかも)
(fset 'lisp-indent-function #'common-lisp-indent-function)

(add-to-list 'auto-mode-alist '("\\.elc" . emacs-lisp-mode))

;; -> xyzzy.el
;; (global-set-key "\C-h" 'delete-backward-char)
;; (define-key isearch-mode-map "\C-h" 'isearch-delete-char)

;;; highlight-line
(require 'hl-line)
(add-hook 'dired-mode-hook 'hl-line-mode)
(add-hook 'buffer-menu-mode-hook 'hl-line-mode)

;;; auto-fill
(setq-default fill-column 80)
(add-hook 'latex-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'auto-fill-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 表示関係
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ツールバーいらない
(tool-bar-mode -1)

;;; ビジブルベル
(setq visible-bell nil)

;;; mark 領域に色付け
(setq transient-mark-mode t)

;;; [UN*X.emacs] から
;;(c-add-style "mystyle" '((c-basic-offset . 4)))
(defun my-indent-style ()
  (c-set-style "gnu")
  (setq c-basic-offset 4))
(add-hook 'c-mode-common-hook   'my-indent-style)
(add-hook 'c++-mode-common-hook 'my-indent-style)

;;; modeline
(set-face-background 'modeline "black")
(set-face-foreground 'modeline "grey90")
(line-number-mode t)
(column-number-mode t)
(setq display-time-string-forms
      '(month"/"day"("dayname") "24-hours":"minutes))
(display-time)

;;; frame
(setq frame-title-format
      `(" %b " (buffer-file-name "( %f )") " on " ,(system-name)
        " ----- " ,(if (featurep 'meadow)
                       (Meadow-version)
                     (format "Emacs %s" emacs-version))))

;;; paren
(show-paren-mode t)			; 対応する括弧に色をつける
;; (setq show-paren-style 'expression) ; カッコ全体をハイライト
(setq parse-sexp-ignore-comments t)	; コメント内のカッコは無視。

(setq inhibit-startup-message t)     ; スタートアップのメッセージを表示しない
;; (setq initial-scratch-message "")    ; *scratch* バッファに入る文字列
(global-font-lock-mode t)		; Syntax Highlighting

;;; タブ、全角スペースを可視化
;;; http://homepage1.nifty.com/blankspace/emacs/color.html
(defface zenkaku-space-face '((t (:background "orange3"))) nil)
(defface highlight-tab-face '((t (:foreground "gray" :underline t))) nil)
(defface url-face '((t (:foreground "steelblue" :underline t))) nil)
(defvar zenkaku-space-face 'zenkaku-space-face)
(defvar highlight-tab-face 'highlight-tab-face)
(defvar url-face 'url-face)
(defadvice font-lock-mode (before my-font-lock-mode activate)
  (font-lock-add-keywords
   major-mode
   `(("\t" 0 highlight-tab-face append)
     ("　" 0 zenkaku-space-face append)
     ("s?https?://[-_.!~*'()a-zA-Z0-9;/?:@&=+$,%#]+" 0 url-face append) ; 簡易URL
     )))
;; (ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)
;; (ad-activate 'font-lock-mode)
(add-hook 'text-mode-hook 'font-lock-fontify-buffer)

;;; バッファのタブ化
(require 'tabbar)
(tabbar-mode (if window-system 1 -1))
;; kill-bufferしたときに戻るバッファが変わるのを抑える
;; http://www.bookshelf.jp/pukiwiki/pukiwiki.php?Elisp%2F%A5%BF%A5%D6%A4%C7%A5%D0%A5%C3%A5%D5%A5%A1%A4%F2%C0%DA%A4%EA%C2%D8%A4%A8%A4%EB
(remove-hook 'kill-buffer-hook 'tabbar-buffer-kill-buffer-hook)
;; グループ化しない
(setq tabbar-buffer-groups-function #'(lambda (buffer) (list "All Buffers"))
      tabbar-buffer-list-function 'valid-buffer-list)
(defsubst valid-buffer-list ()
  ;; (remove-if #'(lambda (buffer) (or (find (aref (buffer-name buffer) 0) " ") (member (buffer-name buffer) '("*Buffer List*" "*Messages*" "*Completions*" "*Compile-Log*")))) (buffer-list))
  (let (acc)
    (dolist (buffer (buffer-list))
      (unless (or (string-match "^ .+" (buffer-name buffer))
                  (member (buffer-name buffer)
                          '("*Buffer List*" "*Messages*" "*Completions*"
                            "*Compile-Log*" "*Dired log*" "*slime-events*"
                            "*Disabled Command*" "*Quail Completions*"
                            "*Fuzzy Completions*" "*WoMan-Log*")))
        (push buffer acc)))
    (nreverse acc)))
(when tabbar-mode
  (set-face-attribute 'tabbar-default-face nil :background "gray60")
  (set-face-attribute 'tabbar-unselected-face nil :background "gray85" :foreground "gray30" :box nil)
  (set-face-attribute 'tabbar-selected-face nil :background "#f2f2f6" :foreground "black" :box nil)
  (set-face-attribute 'tabbar-button-face nil :box '(:line-width 1 :color "gray72" :style released-button))
  (set-face-attribute 'tabbar-separator-face nil :height 0.7)
  (global-set-key [?\C-x ?\C-.] 'tabbar-forward-tab)
  (global-set-key [?\C-x ?\C-,] 'tabbar-backward-tab))

;;; shell-command(M-!) のコマンド入力に補完を効かせる
;;; http://namazu.org/~tsuchiya/
(require 'shell-command)
(shell-command-completion-mode)

;;; エスケープシーケンスを処理する ("ls --color" が使える)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;; shell-modeで上下で補完したい
(require 'shell)
(define-key shell-mode-map [up]   'comint-previous-input)
(define-key shell-mode-map [down] 'comint-next-input)

;;; 現在のバッファを見ながらシェル
(defadvice shell (around shell-split-windows activate)
  (let ((shell-buffer (save-window-excursion ad-do-it)))
    (unless (eq shell-buffer (current-buffer))
      (pop-to-buffer (buffer-name shell-buffer)
                     'other-window))))
;; (defadvice shell (around shell-split-windows activate)
;;   (unless (or (string-match "*\*shell\**" (buffer-name))
;;               (not (y-or-n-p "現在のバッファを残す?")))
;;     (delete-other-windows)
;;     (split-window-vertically)
;;     (other-window 1))
;;   ad-do-it)
(define-key mode-specific-map [?s] 'shell)

;;; URLをブラウザで開く
;; (global-set-key [S-mouse-2] 'browse-url-at-mouse)
(global-set-key "\C-c\C-o" 'browse-url-at-point)
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program
      (cond ((eq system-type 'windows-nt)
             "c:/Program Files/Mozilla Firefox/firefox.exe")
            ((eq system-type 'gnu/linux)
             "x-www-browser"))
      ;; browse-url-generic-args '("-a" "Safari")
      )

;;; バッファの末尾以降の空白が見える (fringe)
(setq-default indicate-empty-lines t)
(set-face-background 'fringe "gray80")

;;; 日付と時刻の挿入
;;; http://www.bookshelf.jp/texi/elisp-manual-20-2.5-jp/elisp_38.html#SEC610
(defun insert-time ()
  (interactive)
  (let ((system-time-locale "C"))
    (insert (format-time-string "%Y-%m-%dT%H:%M:%S"))))
(global-set-key "\C-ct" 'insert-time)

;;; 最近開いたファイル
(recentf-mode t)
;; (global-set-key "\C-c\C-f" 'recentf-open-files) ; なんとなく

;;; gzファイルも編集できるように
(auto-compression-mode t)

;; インデントにtabを使うかどうか
(setq-default indent-tabs-mode nil)

;;; Eldoc は便利です
;; ** Emacs23のeldocは対応する仮引数がハイライトされるようになっている。
;; http://d.hatena.ne.jp/rubikitch/20090207/1233936430
;; (load-file "c:/home/emacs/22.1/lisp/emacs-lisp/eldoc.el")
(and (load-library "eldoc")             ; "~/lib/emacs/eldoc.elc"
     (require 'eldoc-extension))
(setq eldoc-idle-delay 0.2
      ;; 最大化してないとミニバッファがせわしなく動くので注意
      eldoc-echo-area-use-multiline-p
      ;; t
      'truncate-sym-name-if-fit
      )
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(global-set-key "\C-xtl" 'eldoc-mode)

(define-key emacs-lisp-mode-map [?\C-.] 'lisp-complete-symbol)
(define-key lisp-interaction-mode-map [?\C-.] 'lisp-complete-symbol)

;;; カーソルあっちいって
(when (display-mouse-p) (mouse-avoidance-mode 'exile))

;;; Elisp をさらに色付け
(require 'elisp-font-lock)

;;; カーソル上にあるファイル名や URL を開く
;;; フツーの Find file: は C-u C-x C-f
(require 'ffap)
(ffap-bindings)

;;; 関数一覧 (M-x: imenu)
(require 'imenu)
(global-set-key "\C-c\C-l" 'imenu)      ; lisp-modeのslime-load-fileと被る
(global-set-key "\C-ci" 'imenu)

;;; 余分な空白をカット
;;; 標準で 'delete-trailing-whitespace というのがありました
;;; 一応 'query-replace のように確認したほうがいいか？
(defun trim-trailing-whitespace (start end)
  (interactive "*r")
  (save-excursion
    ;; (replace-regexp "[ \t]+$" "" nil start end)
    (perform-replace "[ \t]+$" "" nil t nil nil nil start end)))
(defun trim-whole-tail-whitespace ()
  (interactive "*")
  (trim-trailing-whitespace (point-min) (point-max)))
;; (add-hook 'write-file-hooks 'trim-tail-whitespace)

;;; non-nil なら常に行末移動
(setq track-eol t)

;;; インデントして次の行へ
(defun indent-and-next-line ()
  (interactive "*")
  (indent-according-to-mode)
  (next-line 1))
(define-key global-map "\M-n" 'indent-and-next-line)

;;; 物理行移動マイナーモード
;;; http://xem.jp/~tkng/physical-line.el ; 404 Not Found
;;; http://taiyaki.org/elisp/physical-line/ ; C-e がうまく動かないなぁ
;;; http://www.scythe.jp/lab/physical-line.html ; これが今のとこイイ
(require 'physical-line)
(setq-default physical-line-mode t)
;; (add-hook 'after-init-hook #'(lambda () (with-current-buffer "*scratch*" (physical-line-mode t))))
;; (add-hook 'find-file-hook 'physical-line-mode)

;;; 補完可能なものを随時表示 (少しうるさい)
(icomplete-mode t)

;;; EOF は必ず改行で
(setq require-final-newline t)

;;; マニュアルの文字化け対策
(defadvice man (around man-pages-ja activate)
  (let ((locale-coding-system 'japanese-iso-8bit))
    ad-do-it))

;; 大文字小文字の区別をしない
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;;; 誤爆するとイヤなので無効化
(global-unset-key "\C-xm")		; compose-mail
(global-unset-key "\C-x\C-n")		; set-goal-column

;;; Google 検索
(autoload 'google-search "google" nil t)
(global-set-key "\C-cg" 'google-search)

;;; １行スクロール (でもたまに半画面スクロールする)
'(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 1)

;;; 終了時の状態を保存
(desktop-save-mode t)

;;; スクリプトを保存する時，自動的に chmod +x を行う
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;;; Prolog
(setq prolog-program-name "C:/cygwin/lib/pl-5.6.36/bin/i686-cygwin/pl.exe")
;; (setq prolog-consult-string "[user].\n")
(add-to-list 'auto-mode-alist '("\\.pl$" . prolog-mode))
(defun prolog-consult-buffer ()
  (interactive)
  (save-buffer)
  (prolog-consult-region nil (point-max) (point-min)))

;;; inferior-lisp
;; "c:/usr/local/clisp-2.47-full/bin/clisp.exe"
;; "c:/cygwin/bin/clisp.exe"
(setq inferior-lisp-program
      (cond ((eql system-type 'windows-nt)
             "C:/usr/local/clisp-2.44/clisp.exe")
            ((eql system-type 'gnu/linux)
             "sbcl")))
;; (autoload 'lisp-eval-last-sexp "inf-lisp")
;; (global-set-key "\C-c\C-x" 'lisp-eval-last-sexp)
;; (autoload 'switch-to-lisp "inf-lisp" nil t)
;; (global-set-key "\C-cj" 'switch-to-lisp)

;;; SLIME
(when (eq system-type 'windows-nt)      ; (locate-library "clisp-olio")
  (require 'clisp-olio)
  (defadvice slime-eval-last-expression (before slime-eval-safe+ activate)
    (skip-syntax-forward "w_")
    (when current-prefix-arg
      (indent-for-comment)))

  (define-key slime-repl-mode-map "\C-ch" 'hyperspec-lookup)

  (defun run-gcl (&optional ansi)
    (interactive "P")
    (run-lisp (if ansi
                  "C:/PROGRA~1/GCL-26~1.7-A/lib/GCL-26~1.7/unixport/SAVED_~1.EXE"
                  "C:/PROGRA~1/GCL-26~1.7-C/lib/GCL-26~1.7/unixport/SAVED_~1.EXE")))
  )

;;; newLISP
;; (load-library "newlisp")
(load-library "newlisp/newlisp")
(add-to-list 'auto-mode-alist '("\\.lsp$" . newlisp-mode))

;;; 需要ある？
(global-set-key [?\C-8] 'insert-parentheses)
(global-set-key [?\C-2] (defun insert-double-quotes (&optional arg)
                          (interactive "P")
                          (insert-pair arg ?\" ?\")))
;; (global-set-key [?\(] 'insert-parentheses)
;; (global-set-key [?\"] 'insert-pair)

;; etags は標準で再帰ができない様子
;; 代わりにEmacs標準関数を使おうか
;; #'find-library #'find-variable #'find-function
(setq find-function-C-source-directory
      (if (eq system-type 'windows-nt)
          "c:/home/lxuser/src/emacs-22.2/src/"
          "~/emacs22-22.2/src/"))
(global-set-key [(control ?c) ?f] 'find-library)
(define-key ctl-x-map [?j] 'find-function)

(defun find-symbol-at-point ()
  (interactive)
  ;; (or (find-variable-at-point) (find-function-at-point))
  (let ((sym (symbol-at-point)))
    (cond ((null sym) nil)
          ((boundp sym) (find-variable-other-window sym))
          ((fboundp sym) (find-function-other-window sym))
          (t (error "not found: %s" sym)))))
(define-key esc-map [?.] 'find-symbol-at-point)

;; (load-library "buff-menu")
;; (define-key Buffer-menu-mode-map [(control ?g)] 'quit-window)
;; (add-hook 'buffer-menu-mode-hook #'(lambda () (pop-to-buffer "*Buffer List*")))
(global-set-key [(control ?x) (control ?b)] 'electric-buffer-list)
(setq Buffer-menu-sort-column 2)        ; '(CRM Buffer Size Mode File)
(eval-after-load 'ebuff-menu
  (quote (progn
           (define-key electric-buffer-menu-mode-map "\C-g" 'Electric-buffer-menu-quit))))
;; (global-set-key "\C-x\C-b" 'buffer-menu-other-window)
;; (define-key Buffer-menu-mode-map "\C-g" 'kill-buffer-and-window)

(defun sequence (from to &optional step)
  (if (<= from to)
      (number-sequence from to step)
      (nreverse (number-sequence to from step))))

;;; dired.el
;;; xyzzy.elに移動させる？
(setq ls-lisp-dirs-first t)
(defun dired-here ()
  (interactive)
  (dired default-directory))
(global-set-key (kbd "C-c C-f") 'dired-here)
;; (define-key dired-mode-map [(control ?g)] 'quit-window)
;; filename$ <- 正規表現で検索した方がよさそう
(defadvice dired (around set-position-buffer-file activate)
  "dired起動後、作業中のファイルにカーソルを当てる."
  (let ((file (buffer-file-name)))
    ad-do-it
    (when file
      (let ((name (file-name-nondirectory file)))
        (re-search-forward (concat (regexp-quote name) "$"))
        (backward-char (length name))))))
;; (ad-deactivate 'dired)

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
      (replace-string ") " ")\n")))
  (indent-sexp))

(time-stamp-toggle-active t)
;; ISO 8601 フォーマットにしたいんだが...
(setq time-stamp-format "%:y-%02m-%2dT%02H:%02M:%02S%Z") ; %z,%Zが効かないバグ？
(add-hook 'before-save-hook 'time-stamp)

;; "\M-\;\C-u\C-x\C-e"
;; (eval-last-sexp (universal-argument))は動作後にC-uが残る？
(global-set-key "\M-e" #'(lambda ()
                           (interactive)
                           (indent-for-comment)
                           ;; (ignore-errors (kill-line))
                           (command-execute "\C-u\C-x\C-e")))

(defun recompile-and-load-file ()
  "*.lc があれば再コンパイルとロード."
  (let ((file (buffer-file-name)))
    (when (and (member major-mode '(emacs-lisp-mode lisp-interaction-mode))
               file
               (file-exists-p (byte-compile-dest-file file))
               (null (check-parens)))
      (byte-compile-file file (featurep (intern (pathname-name file)))))))
(add-hook 'after-save-hook 'recompile-and-load-file)
;; (remove-hook 'after-save-hook 'recompile-and-load-file)

(global-set-key "\M-g" 'goto-line)
(global-set-key "\C-xtf" 'toggle-truncate-lines)

;;; toggle-scratch-buffer (で呼び出している pop-to-buffer?) と相性が悪いかも
(require 'linum)

;;; ローカル変数セクションを無視
(setq local-enable-local-variables nil)

;;; @@info
(require 'info)
(add-to-list 'Info-additional-directory-list "~/info/libc/")
(define-key mode-specific-map [?h] 'info-lookup-symbol)

;;; 存在するバッファのみ切り替え
(defadvice switch-to-buffer (before existing-buffer activate compile)
  "When interactive, switch to existing buffers only, unless given a prefix argument."
  (interactive (list (read-buffer "Switch to buffer: "
                                  (other-buffer)
                                  (null current-prefix-arg)))))

;; eshell
(add-hook 'eshell-first-time-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map [(control ?a)] 'eshell-bol)))

(defun detect-and-decode-string (string)
  (mapcar #'(lambda (encoding)
              (list encoding (decode-coding-string string encoding)))
          (detect-coding-string string)))

(or (lookup-key global-map [f12])
    (global-set-key [f12] 'emacs-lisp-mode))
