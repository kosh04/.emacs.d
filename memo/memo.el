;;; -*- mode: lisp-interaction; coding: utf-8 -*-

;; Emacs 21,22,23,24 にて検証したメモ
;; *** 情報が古い可能性あり ***

;; ファイルのカテゴリ分けはこちらを参考
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/

(error "読み込んで使うものじゃないよ！")

;; ls の出力を英語にする "$ LANG=C ls"

;; http://www.namazu.org/~tsuchiya/elisp/
(defadvice insert-directory (around reset-locale activate compile)
  (let ((system-time-locale "C"))
    ad-do-it))

;;; EmacsHelpの呼び出し (help-commandはF1からも呼び出せる)
(global-set-key "\C-c\C-h" 'help-command)
(global-set-key "\M-?" 'help-command)

;;; fill
(setq paragraph-start '"^\\([ 　・○<\t\n\f]\\|(?[0-9a-zA-Z]+)\\)")

;; mew から拝借した(らしい)
(defvar cfirchat-url-regexp
  "\\b\\(s?https?\\|ftp\\|file\\|gopher\\|news\\|telnet\\|wais\\|mailto\\):\\(//[-a-zA-Z0-9_.]+:[0-9]*\\)?[-a-zA-Z0-9_=?#$@~`%&*+|\\/.,]*[-a-zA-Z0-9_=#$@~`%&*+|\\/]"
  "* URL にマッチする正規表現")

;;; shell-mode ではパスワードを表示しない
;;; 多分デフォルト
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)

;;; リドゥ (アンドゥのアンドゥ)
;;; http://www.fan.gr.jp/~ring/Meadow/elisp/redo.el
(require 'redo)
(global-set-key [?\C-_] 'redo)

;;; マウスに反応
(define-key global-map [mouse-4] 'scroll-down)
(define-key global-map [mouse-5] 'scroll-up)
(mwheel-install)
(setq mouse-wheel-follow-mouse t)

;; 色付けに関して (22なら 'jit-lock-mode がデフォ 'lazy-lock-mode)
(setq font-lock-support-mode 'fast-lock-mode)

;;; バッファ間移動
(global-set-key [?\C-x ?\C-.] 'next-buffer)
(global-set-key [?\C-x ?\C-,] 'previous-buffer)

;;; 欲張りな kill-line (空白だけの行は改行もついでに削除する)
(setq kill-whole-line t)

;;; カーソルより前の部分も含めてカット (viのdd)
(global-set-key "\M-k" 'kill-whole-line)

;; ;;; kill-ring はテキスト属性を保存しなくていい (未完成)
;; (defadvice kill-new (around my-kill-ring-disable-text-property activate)
;;   (let ((new (ad-get-arg 0)))
;;     (set-text-properties 0 (length new) nil new)
;;     ad-do-it))

;;; M-x: what-cursor-position (C-x =): カーソル位置の情報


;;; ガーベジコレクトの回数を減らす (デフォルトは 4000000)
(setq gc-cons-threshold 5000000)

;;; mode-info --- 関数・変数の説明文を参照するコマンド
;;; Emacs lisp / C / Perl / Ruby のマニュアルを便利に
;;; http://namazu.org/~tsuchiya/elisp/mode-info.html

;;; 物理行移動
(defun previous-window-line (n)
  (interactive "p")
  (let ((cur-col
	 (- (current-column)
	    (save-excursion (vertical-motion 0) (current-column)))))
    (vertical-motion (- n))
    (move-to-column (+ (current-column) cur-col)))
  (run-hooks 'auto-line-hook))
(defun next-window-line (n)
  (interactive "p")
  (let ((cur-col
	 (- (current-column)
	    (save-excursion (vertical-motion 0) (current-column)))))
    (vertical-motion n)
    (move-to-column (+ (current-column) cur-col)))
  (run-hooks 'auto-line-hook))
(global-set-key "\C-p" 'previous-window-line)
(global-set-key "\C-n" 'next-window-line)
(global-set-key [up] 'previous-window-line)
(global-set-key [down] 'next-window-line)

;;; 正規表現を確認しながら作成 (M-x: re-builder)
(require 're-builder)

;;; M-x: cusomize-apropos

;;; 同一ファイル名のバッファ名を分かりやすく
;;; 片方のバッファを消すとき無駄なバッファが残る
(require 'uniquify)
;;(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-ignore-buffers-re "*[^*]+*")

;;; カラフルな diff M-x: ediff-files
;;; 既に開いているファイル(バッファ)同士でdiffを取るには M-x: ediff-buffers

(when window-system
  (global-set-key [(f9)]
                  #'(lambda ()
                      (interactive)
                      (manual-entry (current-word)))))

;;; (point) 上の単語を見つける
(current-word)
(thing-at-point 'word)

;;; cygwin の man が文字化けしていた原因
;;;  </lisp/man.el>関数 Man-getpage-in-background 744行目あたり
;; (let (...
;;       (coding-system-for-read
;;        (if default-enable-multibyte-characters
;; 	         locale-coding-system 'raw-text-unix))
;;       ...))
;;; うちの環境は locale-coding-system => cp392 だった
(defadvice man (around man-pages-ja activate)
  (let ((locale-coding-system
	 (read-coding-system
	  (format "Coding (default %s): " 'japanese-iso-8bit)
	  'japanese-iso-8bit)))
    ad-do-it))

;;; マニュアルの文字化け対策
(defadvice man (around man-pages-ja activate)
  (let ((locale-coding-system 'japanese-iso-8bit))
    ad-do-it))
;; (ad-deactivate 'man)

;;; よそのウィンドウにカーソルを表示しない
;;; 他のライブラリが上書きしてるかも
(setq cursor-in-non-selected-windows nil)
;; (setq-default cursor-in-non-selected-windows nil)

;;; マウスの位置でなく、カーソルの位置にペーストする
(setq mouse-yank-at-point t)

;; Shift+カーソルでリージョン選択
(pc-selection-mode)

;;; (Meadowではエラー) setq: Spawning child process: exec format error
;; (setq exec-suffixes '(".exe" ".sh" ".pl"))

(defun kitaa ()
  (interactive)
  (let ((kita-list '("ﾟ∀ﾟ" " ﾟ∀" "   ﾟ" "    " "ﾟ   " "∀ﾟ " "ﾟ∀ﾟ")))
    (while (not (input-pending-p))
      (dolist (kao kita-list)
        (message "ｷﾀ━━━(%s)━━━!!!!" kao)
        (sit-for .1)))))

(system-name)                           ; "YOUR-XXXXXXXXXX"
system-time-locale                      ; nil
system-messages-locale                  ; nil
system-configuration                    ; "i386-mingw-nt6.1.7601"
                                        ; "i386-mingw-nt5.1.2600"
system-type                             ; windows-nt
temporary-file-directory                ; "c:/Users/kosh/AppData/Local/Temp/"
                                        ; "c:/tmp/"

;; (split-string (getenv "PATH") path-separator) に近い
(setq exec-path (parse-colon-path (getenv "PATH")))


(defun windows-p ()
  (if (memq system-type '(ms-dos windows-nt)) t nil))

;; `;' や `{' を入力すると自動的に改行される
(add-hook 'c-mode-common-hook 'c-toggle-auto-newline) ; C-c C-a

(getenv "PROGRAMFILES")
;;=> "C:\\Program Files (x86)"
;;=> "C:\\Program Files"

;;; terminal-coding-system が utf-8 を返したら utf-8 な設定にする
(labels ((set-coding-system (c)
           (set-default-coding-systems c)
           (set-terminal-coding-system c)
           (set-keyboard-coding-system c)
           (set-buffer-file-coding-system c)
           (setq default-buffer-file-coding-system c)))
  (set-coding-system (or (cdr (assoc (terminal-coding-system)
                                     '((utf-8 . utf-8)
                                       (japanese-iso-8bit . euc-jp)
                                       )))
                         'utf-8)))

;; 辞書
(labels ((lookup-add-agents (dic path)
           (pushnew (list dic path) lookup-search-agents :test #'equal)))
  (let ((it "c:/meadow/packages/lisp/lookup/"))
    (when (file-exists-p it)
      (pushnew it load-path :test #'equal)
      (require 'lookup)
      (pushnew "c:/meadow/bin/" exec-path :test #'equal)
      (lookup-search-agents 'ndeb "c:/usr/local/dic/EDICT")
      (lookup-search-agents 'ndeb "c:/usr/local/dic/DEVIL")
      (lookup-search-agents 'ndeb "c:/usr/local/dic/ASCDATES")
      (lookup-search-agents 'ndeb "c:/usr/local/dic/JARGON")
      )))

;;; dict:2628 辞書引きプロトコル
(require 'dictem)
;; (setq dictem-server "dict.org" dictem-port 2628)
(eval-after-load "dictem" '(dictem-initialize))
(dictem-run-search ...)
(dictem-run-match )
(dictem-run-define )
(dictem-run-show-server)
(dictem-run-show-info )
(dictem-run-show-databases)

;; @@edict 適当辞書引き
(defvar edict-filename "~/src/edict.gz")
(defun edict (string)
  (interactive "sEdict: ")
  (let ((default-process-coding-system '(euc-jp . euc-jp)))
    (shell-command (format "zgrep -i %s %s" string edict-filename)
                   (get-buffer-create "*edict*"))))

(require 'edict)
(setq *edict-files* '("/usr/share/edict/edict"))

;; n日後を返す関数を返す関数 (どう書く？org)
;; http://ja.doukaku.org/comment/1273/
(defun make-ndays-later (n)
  (lexical-let ((n n))
    (lambda (time)
      (apply (lambda (s mi h d m y dow dst zone)
               (encode-time s mi h (+ d n) m y))
             (decode-time time)))))
(fset 'five-days-later (make-ndays-later 10))

(let ((now (current-time)))
  (cons (format-time-string "%Y/%m/%d %T" now)
        (format-time-string "%Y/%m/%d %T" (five-days-later now))))
;;=> ("2015/02/23 00:05:49" . "2015/03/05 00:05:49")


;; http://cl-cookbook.sourceforge.net/.emacs
;; 中途半端に拡大されるんだが
(defun maximize-frame (&optional frame)
  "Maximize the selected FRAME."
  (interactive)
  (or frame (setq frame (selected-frame)))
  (let ((pixels-per-col (/ (float (frame-pixel-width))
                           (frame-width)))
        (pixels-per-row (/ (float (frame-pixel-height))
                           (frame-height))))
    (set-frame-size frame
                    ;; truncate or round?
                    (truncate (/ (x-display-pixel-width) pixels-per-col))
                    ;; reduce size to account for the toolbar
                    (- (truncate (/ (x-display-pixel-height) pixels-per-row)) 7))
    (set-frame-position frame 0 0)
    ))

;; http://d.hatena.ne.jp/khiker/20090711/emacsfullscreen
;; http://www.emacswiki.org/emacs/FullScreen
(defun fullscreen ()
  (interactive)
  (if (frame-parameter (selected-frame) 'fullscreen)
      (set-frame-parameter nil 'fullscreen 'nil)
      (set-frame-parameter nil 'fullscreen 'fullboth)
      ))
(x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                       '(2 "_NET_WM_STATE_FULLSCREEN" 0))

(require 'linum)
(linum-on)

;; Blogger 関連
;; atom-api (+ nxml)
;; make が失敗するので手動で
;; $ emacs -batch -q -no-site-file -l rng-auto.el -f rng-byte-compile-load
(load "c:/tmp/nxml-mode-20041004/rng-auto")

;;; シンボリックリンク
;;; OSによって挙動が微妙に異なる気が
(make-symbolic-link (buffer-file-name) "NEWNAME.el" t)
(expand-file-name "NEWNAME.el")         ; "c:/home/lxuser/lib/emacs/NEWNAME.el"
(file-symlink-p "NEWNAME.el")           ; nil
(file-truename "NEWNAME.el")            ; "c:/home/lxuser/lib/emacs/NEWNAME.el"
(file-chase-links "NEWNAME.el")         ; "NEWNAME.el"

(file-symlink-p "c:/cygwin/bin/gcc.exe.lnk") ; "c:/cygwin/etc/alternatives/gcc"
(file-truename "c:/cygwin/bin/gcc.exe.lnk")  ; "c:/cygwin/etc/alternatives/gcc"
(file-chase-links "c:/cygwin/bin/gcc.exe.lnk") ; "c:/cygwin/etc/alternatives/gcc"

(file-symlink-p "c:/cygwin/etc/alternatives/gcc.lnk") ; "c:/cygwin/bin/gcc-3.exe"

;; Cygwinの旧リンクも扱いたいならcygwin-emacsを使った方が早いな...

(autoload 'mpg123 "mpg123" "A Front-end to mpg123/ogg123" t)

;; http://www.tsdh.de/repos/darcs/emms/emms-info-mp3info.el

(execute-kbd-macro "\M-\;")
(command-execute "\M-\;")

;; EOF以下のバッファいらない
;; うまく動いてない気がする
(defun fit-window ()
  (interactive)
  (when (pos-visible-in-window-p (point-max))
    (enlarge-window (- (buffer-lines)
                       (window-lines)
                       (get-window-start-line)))))
;; (add-hook 'buffer-menu-mode-hook 'fit-window)

;;; 一発インデント
(defun indent-buffer ()
  "indent current buffer"
  (interactive)
  (indent-region (point-min) (point-max)))

;; docstringに日本語を含むElispファイルをEmacs21でバイトコンパイルすると
;; docstringが文字化けする。場合によってはロードエラーになるので注意
;; Loading error for ~/lib/emacs/xyzzy.elc:
;; (void-variable ias)

;;; @@File-local Variables in Emacs
;;; http://www.kmc.gr.jp/~tak/memo/emacs-local-variable.html

;; Local Variables:
;; coding: utf-8
;; End:

(process-list)                          ; (#<process shell>)
(get-process "shell")                   ; #<process shell>

;; (make-network-process ...)
(process-contact proc)                  ; ("localhost" 4000)

;; マウスホイール移動のときだけ avoidance-mode を無効にしたかったが、出来なかった
(defadvice mwheel-scroll (around no-mouse-avoidance-mode activate)
  (let ((mouse-avoidance-mode nil))
    ad-do-it))

;; @@tabbar-mode
;; http://www.emacswiki.org/cgi-bin/wiki/TabBarMode

;; 概観変更とグループ化の変更
;; http://d.hatena.ne.jp/katsu_w/20080319
;; http://amitp.blogspot.com/2007/04/emacs-buffer-tabs.html

(load-file "~/lib/tabbar.el")
(tabbar-mode 1)
(remove-hook 'kill-buffer-hook 'tabbar-buffer-track-killed)

(setq tabbar-help-on-tab-function
      #'(lambda (tab)
          (if tabbar-buffer-group-mode
              "mouse-1: switch to selected tab in group"
              (let ((name (tabbar-tab-value tab)))
                (or (buffer-file-name (find-buffer name))
                    name)))))

(setq tabbar-select-tab-function
      #'(lambda (event tab)
          "On mouse EVENT, select TAB."
          (let ((buffer (tabbar-tab-value tab)))
            (unless tabbar-buffer-group-mode
              (case (event-basic-type event)
                (mouse-1 (switch-to-buffer buffer))
                (mouse-2 (kill-buffer buffer)) ; (pop-to-buffer buffer t)
                (mouse-3 (delete-other-windows))))
            ;; Disable group mode.
            (setq tabbar-buffer-group-mode nil)
            )))

;; TODO: 外観の調整
(when nil
(set-face-attribute 'tabbar-default nil :background "gray60")
(set-face-attribute 'tabbar-selected nil :background "#f2f2f6" :foreground "black" :box nil)
(set-face-attribute 'tabbar-unselected nil :background "gray85" :foreground "gray30" :box nil)
(set-face-attribute 'tabbar-button nil :box '(:line-width 1 :color "gray72" :style released-button))
;(set-face-attribute 'tabbar-separator nil :height 0.7)
)

(defsubst valid-buffer-list ()
  (let (acc)
    (dolist (buffer (buffer-list))
      ;; *hoge* で表示して得するバッファの方が少ない気がする？
      (unless (or (string-match "^[* ]" (buffer-name buffer))
                  (member (buffer-name buffer)
                          '("*Buffer List*" "*Messages*" "*Completions*"
                            "*Compile-Log*" "*Dired log*" "*slime-events*"
                            "*Disabled Command*" "*Quail Completions*"
                            "*Fuzzy Completions*" "*WoMan-Log*"
                            "*buffer-selection*"))
                  )
        (push buffer acc)))
    (nreverse acc)))

;;; @@regexp-opt -- 正規表現を書くのを支援するツール
(regexp-opt '("define" "lambda" "fn" "define-macro" "lambda-macro") t)
;;=> "\\(define\\(?:-macro\\)?\\|fn\\|lambda\\(?:-macro\\)?\\)"
(regexp-opt '("define" "lambda" "fn" "define-macro" "lambda-macro") 'words)
;;=> "\\<\\(define\\(?:-macro\\)?\\|fn\\|lambda\\(?:-macro\\)?\\)\\>"


;;; @@defadviceマクロ
(ad-is-active 'eval-last-sexp)
;; 活性化/不活性化
(ad-activate ad-deactivate)
(ad-get-arg N)
(ad-set-arg N VALUE)
(setq ad-return-value "セットしたい返り値")

;;; Emacs@vineのカーソル色付けってどうやってたっけ？
(list input-method-activate-hook input-method-inactivate-hook)
(mapcar #'boundp '(mw32-ime-on-hook mw32-ime-off-hook))
(lambda () (set-cursor-color "brown"))

(add-hook 'input-method-activate-hook (lambda () (set-cursor-color "brown")))
(add-hook 'input-method-inactivate-hook (lambda () (set-cursor-color "black")))

(defun input-method-set-cursor-color ()
  (set-cursor-color (if current-input-method "brown" "black")))
(add-hook 'minibuffer-setup-hook 'input-method-set-cursor-color)
(add-hook 'minibuffer-exit-hook 'input-method-set-cursor-color)

;;; @@etags
;; http://www8.atpages.jp/hotsuma/chalow/2002-04-16.html#2002-04-16-1
;; TAGS コマンドまとめ [Emacs]
;; M-x find-tag (M-.) シンボルの定義部分に飛ぶ。
;; M-x pop-tag-mark (M-*) 前の状態に飛ぶ。
;; M-x tags-search シンボルを検索させる。
;; M-x tags-loop-continue (M-,) 次の候補に飛ぶ。(検索状態で実行)
;; M-x find-tag-other-window (C-x 4 .) 定義部分を別ウィンドウに表示。
;; M-x find-tag-other-frame (C-x 5 .) 定義部分を別フレームに表示。
;; M-x complete-symbol (M-TAB) シンボル名の補完(タブテーブルが読み込まれている時)
;; M-x visit-tags-table タブテーブルを読み込み直す。

;; Non-nilならTAGSファイルが更新された時に読み直すか尋ねない
tags-revert-without-query

;; その他etags.elの変数
tags-file-name                          ; "/home/shigeru/src/emacs23/src/TAGS"
tags-table-list                         ; '(...)
;; タグファイルが読み込まれる
(visit-tags-table-buffer)

;; $ etags *.extention
(defun make-tags-file (dir)
  (interactive "DMake TAGS file: ")
  (declare (ignore dir))
  (shell-command (format ;; "%s *.[ch] *.el --output=TAGS"
                  "%s *.el --output=TAGS"
                  (expand-file-name "etags.exe" exec-directory))))

(defadvice find-tag-interactive (before find-current-tags activate)
  ;; カレントディレクトリにTAGSファイルがあればそれを優先する
  (if (file-exists-p #1=(expand-file-name "TAGS"))
      (visit-tags-table #1# "local")))

(apropos "format$" nil)
frame-title-format
header-line-format
mode-line-format
icon-title-format

;; 相対ディレクトリ
(file-relative-name "/usr/bin/sbcl" "/usr")                ; "bin/sbcl"

(defun redraw-emacs ()
  (redraw-display)
  (redraw-frame (selected-frame))
  (redraw-modeline t))

(every #'char-valid-p (number-sequence #x00 #xFF)) ; t

;; 関数、マクロ、変数を未定義化する
;; 依存関係が解消されないとエラー (他のファイルから require されてるとか)
(require 'redo)
(unload-feature 'redo)

;;; @@batch,script
;;; コマンドラインから
command-line-args                       ; ("C:\\home\\emacs\\22.1\\bin\\emacs.exe")
;(setq command-switch-alist '(("-silent")))
;;; スクリプト言語として利用
noninteractive                          ; nil (バッチ処理時 t)
$ emacs --script (Emacs 22 以降のオプション)
$ emacs --batch -Q -l 2> /dev/null
;; --quick, -Q (equivalent to -q --no-site-file --no-splash)
;;; バッチファイルからコンパイル
;;; 後で調べよう
$ emacs -batch -f batch-byte-compile *.el
$ emacs -batch -e "(setq max-specpdl-size 1024)" -f batch-byte-compile *.el
$ emacs -Q --batch --eval '(print (upcase-initials "LL day and night"))'

;; --batch  Edit in batch mode.
;; --script Run file as Emacs Lisp script.

;; 起動時の名前、ディレクトリ
(invocation-name)                       ; "emacs"
(invocation-directory)                  ; "/usr/bin/"
(expand-file-name (invocation-name) (invocation-directory)) ; "/usr/bin/emacs"
installation-directory

;; フルパスで [linux]
(file-truename (format "/proc/%d/exe" (emacs-pid)))
;;=> "/usr/bin/emacs-snapshot-nox"

;;; 再コンパイル
(byte-recompile-directory "~/lib/emacs/" nil)
;;; *.elc のないファイルも強制的にコンパイル
(byte-recompile-directory "~/lib/emacs/" t)

;; ちょっと中途半端過ぎるなあ
(defun copy-elisp-if-newer (fromdir todir)
  (dolist (file (directory-files todir nil "\\.el$"))
    (let ((fromfile (expand-file-name file fromdir))
          (tofile (expand-file-name file todir))
          (byte-compile-verbose nil))
      (when (and (file-newer-than-file-p fromfile tofile)
                 (y-or-n-p (format "%s を更新しますか？" file)))
        (copy-file fromfile tofile 'ok-if-already-exists))))
  (byte-recompile-directory fromdir))

(defun update-elisp-library ()
  (interactive)
  (copy-elisp-if-newer "/windows/home/lxuser/lib/emacs/"
                       "~/lib/emacs/"))

;; @@Comment
;; elispで#|マルチコメント|#は使えないので関数orマクロで囲んでみる
(ignore (...))                          ; 戻り値はnilだが、関数なので引数を評価してしまう
(when nil (...))
(quote (...))
(declare (...))

;; #! がコメント行として無視される
(apply #'+
       #! (error "comment?")
       '(1 2 3 4 5 6 7 8 9 10))         ; 55

;; シンタックステーブルを弄って #! を色付けしたい
;; 1行コメントなんだけど // のような同じ文字(/)構成のものでもない...
;; どうやって定義する？
(let ((syntax lisp-interaction-mode-syntax-table))
  (modify-syntax-entry ?\# "' 1" syntax)
  (modify-syntax-entry ?\! "_ 2" syntax)
  (modify-syntax-entry ?\n "> 34" syntax)
  )

;;; auto-complete-mode
(require 'auto-complete)
(global-auto-complete-mode t)

;; 正規表現の表記
(info "(elisp)Regular Expressions")
(info "(elisp)Syntax of Regexps")

;;; w32console.c
(set-message-beep nil)
(set-message-beep 'asterisk)
(set-message-beep 'exclamation)
(set-message-beep 'hand)
(set-message-beep 'ok)
(set-message-beep 'silent)

;;; Woman (C-x 5 0 :delete-frame)
(defun woman-at-point ()
  (interactive)
  (let ((woman-use-topic-at-point t))
    (woman)))
;; (global-set-key "\C-cw" 'woman-at-point)
(global-set-key "\C-cw" 'manual-entry)  ; できれば Woman よりこっち
(setq woman-use-own-frame nil)

;; @@Completion, 補完等
;; http://www.bookshelf.jp/texi/elisp-manual/21-2-8/jp/elisp_20.html#SEC270
completion-ignore-case                  ; t (補完時の大文字小文字の区別をするか)
(try-completion "f" '("foo" "bar" "baz" "bazz" "hoge" "for"))  ; "fo"
(try-completion "b" '("foo" "bar" "baz" "bazz" "hoge" "for"))  ; "ba"
(all-completions "f" '("foo" "bar" "baz" "bazz" "hoge" "for")) ; ("foo" "for")

(completing-read PROMPT COLLECTION &optional PRED REQUIRE-MATCH INIT HIST DEF INHERIT)
;; require-matchの値に関わらず, 空の入力はつねに許される.

(normal-top-level-add-to-load-path DIRS)

;; ファイルを連番にする
;; ただし、既にソートされている必要あり
(let ((n 0)
      (ext "jpg"))
  (dolist (file (directory-files #1=DIR nil (format "\\.%s$" ext)))
    (rename-file (expand-file-name file #1#)
                 (expand-file-name (format "%04d.%s" n ext) #1#))
    (setq n (1+ n))))

(defun list-to-string (char-list)
  (apply #'string char-list))
(list-to-string (string-to-list "これは漢字文字列")) ; "これは漢字文字列"

;;; help-fns.el:describe-function-1:253
(defun function-truename (def)
  "DEFが関数のエイリアスならば、関数の実体名を返す."
  (while (symbolp (symbol-function def))
    (setq def (symbol-function def)))
  def)

(local-variable-if-set-p 'truncate-lines) ; t
(local-variable-p 'truncate-lines)        ; t

;; http://condotti.blogspot.com/2007_06_01_archive.html
(defsubst buffer-bytes (buffer)
  "Return number of bytes in a buffer."
  (with-current-buffer buffer
    (1- (position-bytes (point-max)))))
;; ファイルサイズとはバッファのバイト数は違うらしい
(list (buffer-bytes "memo.el")
      (buffer-size (get-buffer "memo.el"))
      (nth 7 (file-attributes "memo.el")))
;;=> (102688 91581 102725)

(defun symbol-describe (symbol)
  `((:name     ,(symbol-name symbol))
    (:value    ,(and (boundp symbol)
                     (symbol-value symbol)))
    (:function ,(and (fboundp symbol)
                     (if (byte-code-function-p #1=(symbol-function symbol))
                         symbol #1#)))
    (:plist    ,(symbol-plist symbol))
    (:file     ,(symbol-file symbol))))

(defun describe-hash (table)
  `((:count ,(hash-table-count table))
    (:size ,(hash-table-size table))
    (:weakness ,(hash-table-weakness table))
    (:test ,(hash-table-test table))
    (:rehash-threshold ,(hash-table-rehash-size table))
    (:rehash-threshold ,(hash-table-rehash-threshold table))
    ))

(defun* bindp (key &optional (map global-map))
  (lookup-key map key))

;; $ which chmod
(executable-find "chmod")               ; "c:/cygwin/bin/chmod.exe"

(file-truename (executable-find "x-www-browser"))
"/usr/lib/firefox-3.0.10/firefox.sh"
"/usr/bin/epiphany-gecko"

(user-full-name)
(user-login-name)
user-login-name
(user-original-login-name)              ; alias for `user-login-name'
(user-real-login-name)
(user-real-uid)
(user-uid)

;;; @@Bitwise Operations on Integers
;; lsh (logical shift, 論理シフト)
(lsh #b0101 #b0001)                     ; 10 #b1010
(lsh #b0111 #b0001)                     ; 14 #b1110
(lsh #b0110 -1)                         ;  3 #b0011

;; ash (arithmetic shift, 算術シフト)
(ash #b11111111111111111111111111010    ; -6
     #b11111111111111111111111111111    ; -1
     )                                  ; -3 #b11111111111111111111111111101

;; URL-encode/decode@url-util.el
(url-hexify-string "こんにちは")
;;=> "%e3%81%93%e3%81%b0%e3%82%84%e3%81%97"
(decode-coding-string (url-unhex-string
                       (url-hexify-string "こんにちは"))
                      'utf-8)
;;=> "こんにちは"

;; @@buffer-local バッファローカル考察
;; 新規バッファ立ち上げ時には foo はバッファローカルでない
;; foo の 値は default-foo から受け継ぐ
;; バッファ内で foo に何かしら値を代入すると foo がバッファローカルになる
(setq default-truncate-lines (not default-truncate-lines))
(setq truncate-partial-width-windows (not truncate-partial-width-windows))

(defun SPC->+ (string)
  "Return replaced string which is `space' to `+'"
  (replace-regexp-in-string "[[:space:]]+" "+" string))

;; [A-Za-z] == [[:alpha:]]
;; [A-Za-z0-9] == [[:alnum:]]

;; http://cadr.g.hatena.ne.jp/g000001/20090221/1235217251
(defmacro define-command (name type args &rest body)
  (let ((type (case type
                (:numeric "p")
                (:numeric/raw "P")
                (:region "r")
                (otherwise "type"))))
    `(defun ,name ,args
       (interactive ,type)
       ,@body)))
(define-command hello :region (start end)
  (message (format "%d, %d" start end)))

;; (kbd "C-]")
(defadvice abort-recursive-edit (before with-delete-temp-buffer activate)
  (kill-buffer (get-buffer-create "*Completions*")))

(lookup-words "lisp")

;; @@CLパッケージの関数が使えないときのelisp関数
;; もしくはCL,loopマクロ (展開されれば純elispコード)
(position ?. "This is STRING.")         ; 14
(string-match (regexp-quote (string ?.)) "This is STRING.") ; 14
(funcall (lambda (item seq)
           (catch #1='found
             (do ((len (length seq))
                  (idx 0 (1+ idx)))
                 ((= idx len) nil)
               (if (eq item (elt seq idx))
                   (throw #1# idx)))))
         ?. "This is STRING.")          ; 14

(substitute ?E ?e "emacs")              ; "Emacs"
(subst-char-in-string ?e ?E "emacs")    ; "Emacs"

(format nil "~{~A~^, ~}" '(Windows UNIX Mac Solaris))
(mapconcat #'symbol-name '(Windows UNIX Mac Solaris) ", ")
;;=> "Windows, UNIX, Mac, Solaris"

(some (lambda (x) DO-SOMETHING) SEQ)
(dolist (x SEQ)
  (if DO-SOMETHING (return x)))

;; 比較を使う関数は `equal' で比較するものが多い
;; `eq' で比較する関数は XXXq
'((assoc . assq)
  (delete . delq)
  (member . memq)
  (rassoc . rassq)
  (remove . remq))

;; 文字型の関数は文字列型の関数で代用できる
;; position
(string-match (string ?\n) "Hello\nWorld") ; 5

;; @@emacs<->xyzzy

;; defun* (cl-macs.el) の使い道が分からない
;; - 使えるラムダキーワードが増える lambda-list-keywords (&key &body など)
;; - 引数のデフォルト値を指定できる
;; - つまり、よりCLっぽいdefunになる

;; Emacs Lispで関数定義を置き換える方法
;; - http://d.hatena.ne.jp/rubikitch/20090918/1253265439
;; letf

;; interactiveの挙動
;; emacs => 1 / xyzzy => nil
(call-interactively (lambda (&optional arg)
                      (interactive "p")
                      arg))
;; emacs => ERR / xyzzy => 1
(call-interactively (lambda (&optional (arg 1))
                      (interactive "p")
                      arg))

;; emacsが生成した子プロセスには名前がつけられる
(start-process NAME BUFFER ...)
(get-process NAME)
;; xyzzyにはない (プロセスに関連付けされているバッファがあるのは共通)
(make-process CMD &key ENVIRON ...)
(buffer-process BUFFER)

;;; 最近開いたファイル
;; M-x: recentf-open-files
;; [menu-bar] File -> Open Recent
(recentf-mode t)

;; 迂闊に使えないようになっているコマンド群
;; (put SYMBOL 'disabled nil) で解除
(with-output-to-temp-buffer "*disabled command*"
  (mapatoms #'(lambda (symbol)
                (when (get symbol 'disabled)
                  (prin1 symbol)
                  (terpri)))))

;; Display Time
(setq display-time-string-forms '(month"/"day"("dayname") "24-hours":"minutes))
(display-time)

(setq display-time-format "%Y-%m-%d(%a) %H:%M")
(setq display-time-default-load-average nil
      display-time-24hr-format t
      display-time-day-and-date t)
(let ((system-time-locale "C"))
  (display-time-mode))

;; M-x world-clock
(setq display-time-world-list
      '(("America/Los_Angeles" "シアトル")
        ("America/New_York" "ニューヨーク")
        ("Etc/UTC" "UTC")
        ("Europe/Athens" "アテネ")
        ("Asia/Shanghai" "上海")
        ("Asia/Hong_Kong" "香港")
        ("Asia/Tokyo" "東京")
        ("Pacific/Auckland" "オークランド")
        )
      display-time-world-time-format "%FT%T%z"
      )

;;; 端末から起動した時 (emacs -nw) にメニューバーを消す
(menu-bar-mode (if window-system 1 -1))

;;; non-nil なら常に行末移動
(setq track-eol t)

;;; Prolog
(setq prolog-program-name "C:/cygwin/lib/pl-5.6.36/bin/i686-cygwin/pl.exe")
(setq prolog-consult-string "[user].\n")
(add-to-list 'auto-mode-alist '("\\.pl$" . prolog-mode))
(defun prolog-consult-buffer ()
  (interactive)
  (save-buffer)
  (prolog-consult-region nil (point-max) (point-min)))

;; もう一つのPorlogモード
;; Bruda.CA - Prolog mode for (X)Emacs - http://bruda.ca/emacs-prolog/

;; @@unicode
;; http://www.unicode.org/Public/UNIDATA/UnicodeData.txt
;; C-u M-x: what-cursor-position の表示が少し増える
;; 内部コードがunicodeなemacs23には必要あるの？->それとこれは多分別問題
(setq describe-char-unicodedata-file "~/src/UnicodeData.txt")

;; ロケールを一時的に変更
;; time-stampなどに使える
(let ((system-time-locale "C"))
  ...)

(current-time-string)                   ; "Fri Jul 31 03:59:30 2009"

;; http://mail.ring.gr.jp/skk/200704/msg00018.html
(string-lessp "ー" "あ")                ; t (Emacs22)

;; 細長カーソル
(setq cursor-type 'bar)
(setq cursor-type t)

;; @@eshell
(add-hook 'eshell-first-time-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map [(control ?a)] 'eshell-bol)))

;; eshellを普通に使っていたらホームディレクトリ以下が全部消えた話 - 半隠遁日記
;; http://d.hatena.ne.jp/Rommy/20070115/1168876829
(setq eshell-glob-include-dot-dot nil)

;; callproc.c
(list buffer-file-type
      shell-file-name
      exec-path
      exec-suffixes
      exec-directory
      data-directory
      configure-info-directory
      shared-game-score-directory
      temp-file-name-pattern
      process-environment
      )
(safe-length )

(require 'xyzzy)
(decode-coding-string (si:www-url-decode
                       (si:www-url-encode "エンコード"))
                      locale-coding-system)
;;=> "エンコード"

;; RFC***.el
(let (acc)
  (dolist (dir load-path)
    ;; "rfc[0-9]+\\.el$"
    (dolist (file (directory-files dir nil "rfc[[:digit:]]+\\.el$"))
      (push file acc)))
  (nreverse acc))
;;=> ("rfc2368.el" "rfc822.el" "rfc1843.el" "rfc2045.el" "rfc2047.el" "rfc2104.el" "rfc2231.el")

(bound-and-true-p explicit-shell-file-name) ; "bash"

;; ミニバッファリサイズの挙動 (nil t grow-only)
resize-mini-windows

;; ファイルのロード中に(exit)関数があるとうれしい気がするんだが
;; インデントされたくない
(when (boundp 'hoge)
  ...)
;; よりも
(when (boundp 'hoge)
  (return-from-load))
...
;; の方が見やすいのではないか。そうでもないか

;; (apropos "load") から
current-load-list
;;=>
((defun . set-buffer-fold-width)
 (defun . back-to-mini)
 (defun . back-to-mini)
 (defun . back-to-mini)
 (defun . back-to-mini)
 (defun . back-to-mini)
 (defun . back-to-mini)
 (defun . back-to-mini)
 (provide . make-network-process))

;; emacs組み込みでロードされるファイル一覧
;; $ emacs -Q --batch --eval '(print preloaded-file-list)'
preloaded-file-list
;;=>
("site-init" "tooltip" "ediff-hook" "vc-hooks" "emacs-lisp/float-sup" "x-dnd" "tool-bar"
 "mwheel" "dnd" "international/fontset" "image" "fringe" "buff-menu" "abbrev" "replace"
 "textmodes/fill" "textmodes/text-mode" "emacs-lisp/lisp-mode" "textmodes/paragraphs"
 "register" "textmodes/page" "emacs-lisp/lisp" "paths.el" "menu-bar" "rfn-eshadow" "isearch"
 "emacs-lisp/timer" "select" "scroll-bar" "mouse" "jit-lock" "font-lock" "emacs-lisp/syntax"
 "facemenu" "font-core" "term/tty-colors" "frame" "window" "indent" "international/ucs-tables"
 "language/georgian.el" "language/utf-8-lang.el" "language/misc-lang.el" "language/vietnamese"
 "language/tibetan" "language/thai.el" "language/lao.el" "language/korean.el" "language/japanese.el"
 "language/hebrew.el" "language/greek.el" "language/romanian.el" "language/slovak.el"
 "language/czech.el" "language/european" "language/ethiopic" "language/english.el"
 "language/kannada.el" "language/tamil.el" "language/malayalam.el" "language/devanagari.el"
 "language/indian" "language/cyrillic" "language/chinese" "international/latin-9.el"
 "international/latin-8.el" "international/latin-5.el" "international/latin-4.el"
 "international/latin-3.el" "international/latin-2.el" "international/latin-1.el"
 "international/characters" "international/utf-16" "international/utf-8"
 "case-table" "international/mule-cmds" "jka-cmpr-hook" "help" "simple" "loaddefs.el"
 "startup" "button" "faces" "cus-face" "files" "bindings" "format"
 "international/mule-conf.el" "international/mule" "cus-start" "env" "emacs-lisp/map-ynp"
 "custom" "widget" "version.el" "subr" "emacs-lisp/backquote" "emacs-lisp/byte-run" "loadup.el")

;; M-x: compile のデフォルトコマンド
compile-command                         ; "make -k "

;;; @@Minibuffer

;; space でファイル名補完
(define-key minibuffer-local-filename-completion-map " " 'minibuffer-complete-word)
(define-key minibuffer-local-must-match-filename-map " " 'minibuffer-complete-word)

(define-key completion-list-mode-map [tab] 'next-completion)
(define-key completion-list-mode-map [backtab] 'previous-completion)
(define-key completion-list-mode-map [?q] 'quit-window)

;; シンボル補完に xxx? も含めたい
;; もしくは C-q ?
(define-key minibuffer-local-completion-map "?" 'self-insert-command)

;;; @@Minor-Modes
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Minor-Modes.html
;; A positive argument always turns the mode on, and explicit zero
;; argument or a negative argument always turns it off.
;; * マイナーモードの切り替えは t/nil ではなく +/-(0)

;; 行間
(setq default-line-spacing nil)

(getenv "EMACSDATA")                    ; "C:\\meadow\\etc"
(getenv "EMACSPATH")                    ; "C:\\meadow\\bin"
(getenv "EMACSLOADPATH")                ; "C:\\meadow\\site-lisp;C:\\site-lisp;C:\\meadow\\packages\\lisp;C:\\meadow\\lisp;C:\\meadow\\leim"
(getenv "EMACS_DIR")                    ; "C:/meadow"
(getenv "EMACSDOC")                     ; "C:\\meadow\\etc"
(getenv "TERM")

;;; @@Network Function
;; built-in function
(make-network-process &rest ARGS)
(set-network-process-option PROCESS OPTION VALUE &option NO-ERROR)
(format-network-address ADDRESS &optional OMIT-PORT)
(format-network-address [127 0 0 1])    ; "127.0.0.1"
(format-network-address [127 0 0 1 0])  ; "127.0.0.1:0"
(format-network-address "lo")
network-coding-system-alist

(network-interface-list)   ; (("lo" . [127 0 0 1 0]) ("eth0" . [192 168 3 3 0]))
(network-interface-info "eth0")

;; アドレス一覧
(mapcar (pcase-lambda (`(,ifname . ,ip))
              `(,ifname . ,(format-network-address ip)))
            (network-interface-list))

;; lisp/net/net-utils.el
(featurep 'net-utils)
(network-connection HOST PORT)
(network-connection-to-service HOST SERVICE)
(open-network-stream NAME BUFFER HOST SERVEICE)
(network-connection HOST PORT)          ; telnet みたいなの

(error "なにかしらエラー")
;; [*Backtrace*]
;; Debugger entered--Lisp error: (error "\xd24a\xd24b\xd22b\xd237\xd269\xd2a8\xd2e9\xd0bc")
;; どうやらemacs-muleエンコーディングらしい
"\xd24a\xd24b\xd22b\xd237\xd269\xd2a8\xd2e9\xd0bc" ;=> "なにかしらエラー"

(setq print-escape-newlines (not print-escape-newlines))
(message "Hello\n")

(setq print-escape-multibyte (not print-escape-multibyte))
(message "おはよう")

;; エスケープシーケンスの見た目を制御
;; t->"^@^A^B^C^D^E", nil->"\001\002\003\004\005"
(setq ctl-arrow (not ctl-arrow))        ; or default-ctl-arrow
(concat [0 1 2 3 4 5])                  ; " "

;; @@Stream, Emacsの入出力ストリーム
;; buffer marker function t(echo area) nil(standard-output) symbol
(info "(elisp) Input Streams")
(info "(elisp) Output Streams")

standard-output                         ; #<buffer memo.el> (eval-print-last-sexp)
standard-output                         ; t (eval-last-sexp)

(princ initial-scratch-message (get-buffer "*scratch*"))

(let (last-output)
  (princ "HAL-8000" (lambda (c)
                      (push (1+ c) last-output)))
  (concat (reverse last-output)))       ; "IBM.9111"


;; @@comint.el
;; 挿入した文字列の長さだけマーカーも移動したい
(set-marker comint-last-input-start (point))
(set-marker comint-last-input-end (point))
(set-marker comint-last-output-start (point))
(set-marker comint-accum-marker nil)

;; プログラムのソース<->ヘッダの切り替え
M-x: ff-find-other-file

;; [Edit]->[Copy]
;; キルリングとクリップボードの中途半端な同期
(global-set-key (kbd "ESC w") 'clipboard-kill-ring-save)

;; call-processがcygwinのシンボリックリンクを辿らないのがよろしくないな
;; cygwin-emacsでなくNTemacsだから当然といえば当然だが
(call-process "/bin/pwd")               ;=> 0
(call-process "/bin/ispell")            ; ERR: "no such file or directory"
;; シェルを仲介させる手もある
(call-process "sh" nil nil nil "-c" "/bin/ispell") ;=> 0

(locate-file "ispell" exec-path (cons ".lnk" exec-suffixes) 0)
;;=> "c:/cygwin/bin/ispell.lnk"

;;; isearch で IME をオフにする
;;; http://www-tsujii.is.s.u-tokyo.ac.jp/~yoshinag/tips/elisp_tips.html#ime-off

;; ファイル保存後に改行を付加するのは勘弁してくれ
(setq require-final-newline nil)
;; t			改行付加あり
;; nil			改行付加なし
;; t以外のnon-nil	保存時に尋ねる
(setq mode-require-final-newline nil)

;; tabbarのタブを中クリックしたらバッファを削除したい
(lookup-key global-map  [header-line down-mouse-1]) ; mouse-drag-header-line

;; マウスクリックでURLを開く
(global-set-key [mouse-1] 'ffap-at-mouse)

(defun region-string ()
  "リージョンが存在するなら、その文字列を返す."
  (and mark-active
       (buffer-substring (region-beginning) (region-end))))

(compare-buffer-substrings buffer1 start1 end1 buffer2 start2 end2)

;; つかえないよ
(defun switch-to-mode (mode)
  (interactive (list (completing-read "Mode: "
                                      (-map (-lambda ((re . mode))
                                              (if (consp mode) (cadr mode) mode))
                                            auto-mode-alist)
                                      nil t)))
  (funcall mode))

;;; TODO: .emacsの整理
22,23で共通するもののみ
他は .emacs22, .emacs23 とか
外部ライブラリに依存するものも外す
普段使わないどうでもいい関数はmemo.elに
windows/linuxでパス名が異なるのも何とかしたい (setq hoge-command "/bin/bash")


(defvar radix64 "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=")
(mapconcat #'string
           (append (number-sequence ?A ?Z)
                   (number-sequence ?a ?z)
                   (number-sequence ?0 ?9)
                   '(?+ ?/ ?=))
           nil)
;; C-x C-@ runs the command pop-global-mark
global-mark-ring

;; 連番
(command-execute (kbd "<f3> <f3> <RET> <f4> C-p C-k C-u 10 <f4>"))
(dotimes (n 10) (command-execute [f3 13]))
(shell-command "seq 1 10")

;; ターミナル上で動いているアプリケーションには
;; ターミナル経由で文字コードしか渡せません。
;; ターミナルでC-,やC-.に割り当てたい - (ひ)メモ - http://d.hatena.ne.jp/hirose31/20040219/1160403206

;; 飽きた
(defun describe-attributes (filename)
  (multiple-value-bind (linked-dir links  uid gid lat lmt lsct size modes gid-if inode device)
      (values-list (get-file-attributes filename))
    `((:linked-dir linked-dir)
      (:number-of-links links)
      )))

;; (define-key emacs-lisp-mode-map [?\C-.] 'lisp-complete-symbol)
;; (define-key lisp-interaction-mode-map [?\C-.] 'lisp-complete-symbol)

;;; １行スクロール (でもたまに半画面スクロールする)
(setq scroll-conservatively 35 scroll-margin 0 scroll-step 1)

;; TAB
(set-variable (quote tab-width) 4)             ; タブ幅変更 (バッファローカル)
(set-default 'tab-width 4)                     ; (グローバル)

;; 印刷 (ps-print)
(setq ps-multibyte-buffer 'non-latin-printer)
(ps-print-buffer-with-faces FILENAME)

(let ((explicit-shell-file-name "cmd.exe"))
  (shell))

(let ((buffer-read-only nil))          ; inhibit-read-only
  ...)

;; (define-key help-map (kbd "C-x C-c") 'kill-emacs)
;; (setq confirm-kill-emacs 'yes-or-no-p)

;; Emacs23のC-lの挙動が気に入らない
(if (eq (lookup-key (current-global-map) "\C-l") 'recenter-top-bottom)
    (global-set-key "\C-l" 'recenter))

;; http://nijino.homelinux.net/emacs/subst-jisx0208ex.el

;; スーパーユーザーで編集 (tramp)
;; /sudo::/boot/grub/menu.lst
;;

(equal "\376\377\"`" "\xfe\xff\x22\x60")          ; t
(decode-coding-string "\376\377\"`" 'utf-16)      ; "≠"

;; 休日にマーク…って日本時間じゃない
(let ((mark-holidays-in-calendar t)) (calendar))

;; emacs@ms-dos
(global-set-key [enlw] 'toggle-input-method)
(global-set-key [auto] 'toggle-input-method)

(global-set-key "\C-x\C-d" nil)         ; list-directory

(subr-arity (symbol-function 'apply))   ; (2 . many)

(defun process-describe (process)
  `((name  ,(process-name process))
    (status  ,(process-status process))
    (id  ,(process-id process))
    (buffer ,(process-buffer process))
    (coding-system ,(process-coding-system process))
    (command ,(process-command process))
    (contact ,(process-contact process))
    (datagram-address ,(ignore-errors (process-datagram-address process)))
    (exit-status  ,(process-exit-status process))
    (filter  ,(process-filter process))
    (sentinel  ,(process-sentinel process))
    (inherit-coding-system-flag  ,(process-inherit-coding-system-flag process))
    (mark  ,(process-mark process))
    (plist  ,(process-plist process))
    (query-on-exit-flag  ,(process-query-on-exit-flag process))
    (tty-name  ,(process-tty-name process))
    ))


;; @@isearch
;; 効いてる？
(defadvice isearch-forward (around fep-off activate)
  (let ((current-input-method nil))
    (update-mode-line)
    ad-do-it))

;; 元のバッファはそのまま
(defun save-buffer-as (filename)
  "名前を付けてバッファを保存."
  (interactive "FSave as: ")
  (let ((obuf (current-buffer)))
    (with-temp-buffer
      (insert-buffer-substring obuf)
      (write-file filename))))
(defalias 'save-buffer-noselect #'save-buffer-as)

;; @@w3m
;; 名前を付けてページを保存: w3m-download
;; 名前を付けてリンク先を保存: w3m-download-this-url
;; 文字コードを指定して再読み込み: ??

;; さらに短縮
(defun w3m-save-page-as (filename)
  (interactive (list (read-file-name
                      "FSave page as: "
                      (file-name-as-directory w3m-default-save-directory)
                      nil nil
                      (file-name-nondirectory
                       (w3m-url-strip-query w3m-current-url)))))
  (w3m-download w3m-current-url filename))

;; emacs23ではurl-handlerの実装が不十分
(defun url-file-name-completion (url directory &optional predicate)
  nil)

;; (url-handler-mode 1)
;; url-handler-mode@emacs23がまともに動けばこんなことする必要もないのだが...
(defun get-url (url)
  "指定したURLをバッファに表示する."
  (interactive "sURL: ")
  (let ((buffer (url-retrieve-synchronously url)))
    (unless buffer
      (error "Opening input file: No such file or directory, %s" url))
    (switch-to-buffer buffer)
    (rename-buffer (file-namestring url))
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (toggle-read-only)
    (dolist (mode auto-mode-alist)
      (if (string-match (car mode) url)
          (return (funcall (cdr mode)))))
    (setq mode-line-format default-mode-line-format)))

;; (defun rename-current-file (newname)
;;   (interactive "FNew file: ")
;;   (rename-file (buffer-file-name (current-buffer)) newname)
;;   (rename-buffer (file-name-nondirectory newname)))

;; エディタの折りたたみ機能
hs-minor-mode
http://www.emacswiki.org/emacs/hideshowvis.el

(defun last-command-eq (x)
  (eq last-command x))
(eq last-command this-command)

;; デスクトップの復元
(desktop-read)

(defun my:session-restore ()
  "Restore a saved emacs session."
  (interactive)
  (if (file-exists-p (desktop-full-file-name))
      (desktop-read)
    (message "No desktop found.")))
(defalias 'session-restore 'my:session-restore)

;; memory
(memory-limit)                          ; 176388
(memory-use-counts)                     ; (4727087 3839 2250706 66553 13555285 203001 181952 733908)

;; memberはリストの検索しか出来ない(CLを使わない場合)
;; ベクタの中にあるシンボルの検索はどうする？
(dotimes (i (length obarray))
  (if (fboundp #1=(aref obarray i))
      (return #1#)))

;; Interact with the lisppaste pastebot via xml-rpc
;; http://www2.ph.ed.ac.uk/~s0198183/lisppaste.el

;; ELPA - the Emacs Lisp Package Archive.
;; http://tromey.com/elpa/index.html

;; EmacsWiki: install-elisp.el
;; http://www.emacswiki.org/emacs/install-elisp.el

;; FILENAMEの開くべき方法を制御する (before find-file)
file-name-handler-alist

(type-of #[])                           ; compiled-function
(type-of [])                            ; vector

;; 文字列内の${STRING}を置き換え
(substitute-in-file-name "username=$USERNAME")
(with-setenv '(("XYZZYHOME" . "/home/xyzzy/"))
  (substitute-in-file-name "${XYZZYHOME} -e 'hello world'"))
;;=> "/home/xyzzy/ -e 'hello world'"

;; 文字列中のシンボルを置換 (キーマップ、コマンドのみ)
;; \[COMMAND] -> "M-x COMMAND"
(substitute-command-keys
 "To abort recursive edit, type: \\[abort-recursive-edit]")
;; \{KEYMAP} -> ? (describe-bindings KEYMAP)
(substitute-command-keys "\\{minibuffer-local-must-match-map}")
(substitute-command-keys "\\<minibuffer-local-must-match-map>")

;; http://edward.oconnor.cx/2006/03/json.el
(url-load "http://edward.oconnor.cx/elisp/json.el")
(eval-after-load "json"
  (quote (progn
           (fset 'json-decode #'json-read-from-string)
           )))

(defun %natnump (obj)
  "Return t if OBJECT is a nonnegative integer."
  (and (integerp obj) (< 0 obj)))

;; with-temp-file@emacsはファイルを削除しない
(let ((tempfile (make-temp-file "emacs" nil ".tmp")))
  (unwind-protect
       (progn
         (with-temp-file tempfile
           (insert "hello world"))
         (message "%s:%s" tempfile
                  (with-temp-buffer
                    (insert-file-contents tempfile)
                    (buffer-string))))
    (and (file-exists-p tempfile)
         (delete-file tempfile))))
;; "/tmp/emacs30476up2.tmp:hello world"

;; FILENAMEに文字が書き出される
(with-temp-file "~/output.el"
  (insert "Hello World.\n" ))

;; 関数の中身を表示
(indirect-function 'car)                ; #<subr car>
(indirect-function 'when)               ; (macro . #[...])

;; 上記の関数となにが違う？
(symbol-function 'car)                  ; #<subr car>
(symbol-function 'when)                 ; (macro . #[...])

;; コメントのスタイルを変更
;; Cなどの複数行コメントに有効
(setq comment-style 'multi-line)
comment-styles
((plain nil nil nil nil)
 (indent nil nil nil t)
 (aligned nil t nil t)
 (multi-line t nil nil t)
 (extra-line t nil t t)
 (box nil t t t)
 (box-multi t t t t))

(defmacro define-comment-dwim-with-style (style)
  `(defun ,(intern (format "comment-dwim-with-%s" style)) (arg)
     (interactive "*P")
     (let ((comment-style ',style))
       (comment-dwim arg))))
(define-comment-dwim-with-style plain)
(define-comment-dwim-with-style indent)
(define-comment-dwim-with-style aligned)
(define-comment-dwim-with-style multi-line)
(define-comment-dwim-with-style extra-line)
(define-comment-dwim-with-style box)
(define-comment-dwim-with-style box-multi)

;; URLから参照できるライブラリ
"http://www.emacswiki.org/emacs/download/install-elisp.el"

;; テキストをHTMLに変換
;; http://fly.srk.fer.hr/~hniksic/emacs/htmlize.el.html
"http://fly.srk.fer.hr/~hniksic/emacs/htmlize.el"

;; textdoc
;; AsciiDoc mode
;; http://xpt.sourceforge.net/tools/
"http://xpt.sourceforge.net/tools/doc-mode/doc-mode.el"

;; markdown-mode
;; http://jblevins.org/projects/markdown-mode/
"http://jblevins.org/projects/markdown-mode/markdown-mode.el"

;; @@grep
(defun zgrep ()
  (interactive)
  (let ((grep-command "zgrep -nH -e  *.gz"))
    (call-interactively 'grep)))

;; `grep' より `lgrep' の方が親切

;; /usr/bin/lgrep は日本語検索が可能な Grep(lv)

(kill-process )  ; Kill process PROCESS.  May be process or name of one.
(delete-process) ; Delete PROCESS: kill it and forget about it immediately.

;; gist.github の生テキスト
"http://gist.github.com/${gistid}.txt"

;; sdic@ubuntu-emacs がどうしてデフォルトで使えないのか
(setq sdic-eiwa-dictionary-list
      '((sdicf-client "/usr/share/dict/gene.sdic")))
(global-set-key "\C-ce" 'sdic-describe-word-at-point)
(sdic-search-eiwa-dictionary "lisp")

;; autoloadモジュールの一覧
(let (acc)
  (do-symbols (x)
    (and (fboundp x)
         (autoload-function-p x)
         (pushnew (nth 1 (symbol-function x))
                  acc :test #'equal)))
  (nreverse acc))
;;=> ("elisp-slime-nav" "man" "rmail" "conf-mode" "sql" "printing" "kinsoku" ...)


;; リモートのファイル、ディレクトリを元のパスに変換したい
"/ftp:repl@es.land.to:/public_html/index.html" ; emacs (Tramp,Ange-ftp)
"ftp://repl@es.land.to/public_html"            ; nautilus
"ftp://repl.es.land.to/index.html"             ; ブラウザ

;; Java Development Environment for Emacs
;; http://jdee.sourceforge.net/

(defvar elisp-base-directory (file-name-directory (locate-library "simple")))
(defvar elisp-compressed-p (if (locate-library "simple.el.gz") t))
(defun elisp-grep-find ()
  (interactive)
  (let ((default-directory elisp-base-directory)
        (grep-find-command
         (if elisp-compressed-p
             "find * -type f -print0 | xargs -0 -e zgrep -nH -e {} *.gz"
             "find * -type f -print0 | xargs -0 -e grep -nH -e {} *")))
    (call-interactively 'grep-find)))

;; 多機能テンプレートモード
;; http://code.google.com/p/yasnippet/

(decode-coding-string "\x53\x30\x93\x30\x6B\x30\x61\x30\x6F\x30" 'utf-16le)
(decode-coding-string "\xA4\xB3\xA4\xF3\xA4\xCB\xA4\xC1\xA4\xCF" 'euc-jp)

(char= ?Ａ ?Α ?А)                     ; nil

(char-name ?Ａ)                         ; "FULLWIDTH LATIN CAPITAL LETTER A"
(char-name ?Α)                         ; "GREEK CAPITAL LETTER ALPHA"
(char-name ?А)                         ; "CYRILLIC CAPITAL LETTER A"

(describe-char-unicode-data ?А)

;; あまり賢くない
(defun nkf-g (filename &optional textp)
  "漢字コード判別"
  (cond (textp
         (car (detect-coding-string filename)))
        (:else
         (with-temp-buffer
           (insert-file-contents filename)
           buffer-file-coding-system))))
(nkf-g "~/tmp/encoding_test/utf32")     ; mule-utf-16be-with-signature
(nkf-g "memo.el")                       ; mule-utf-8-unix
(nkf-g (encode "こんにちは" 'euc-jp) t) ; japanese-iso-8bit

;; TODO: ミニバッファで M-x よりも shell 使えた方が便利なんじゃないかと
;; それ、(require 'shell-command) で出来るな。
;; もしくはシェルのない環境用に eshell とか？

;; TODO: プリミティブな変数かどうかを調べるには？
standard-input
(subrp (indirect-function 'car))

;; @@shell-like
;; 既に同名で定義されているもの: pwd cd
(fset 'rm #'delete-file)
(fset 'ls #'dired)                      ; or #'list-directory
(fset 'cat #'find-file)

;; ミニバッファで FILENAME.C を補完する際に
;; Find file: ~/FILENAME.c [Sole completion]
;; 大文字に変換してくれないバグ？ -> emacs23 で解決

;; ファイルまたはディレクトリの中身を有無をいわさず削除する
(dired-delete-file FILE &optional RECURSIVE)
(defun rm-rf (file)
  (cond ((not (eq t (car (file-attributes file))))
         (delete-file file))
        (t
         (dolist (f (directory-files file t dired-re-no-dot))
           (rm-rf f))
         (delete-directory file))))

;; ワイルドカード->正規表現
(wildcard-to-regexp "*.[ch]") ;=> "\\`[^ ]*\\.[ch]\\'"

;; let で束縛しても効果が見られない
(defadvice isearch-mode (around input-method-off activate)
  (let ((current-input-method nil))
    (force-mode-line-update)
    (isearch-update)
    ad-do-it))
(ad-deactivate 'isearch-mode)

;; ミニバッファに入力がなければ利用する
;; http://www.fan.gr.jp/~ring/doc/elisp_20/elisp_21.html#IDX879
(unless (input-pending-p)
  (message "hello"))

;; y-n以外でQuitも有効にしたい
;;         端末から  Xから
;; Emacs22 o         x
;; Emacs23 o         x
;; なぜウィンドウから利用するとQuitが効かなくなるんだろうか
(y-or-n-p "ok? ")
;; Caps(Ctrl)-g が原因かもしれない

(defun following-char () (or (char-after) 0))
(defun preceding-char () (or (char-before) 0))

;; 一時バッファに色が付かなくなるのはなんで？
(defun lisp-fontify-string (string)
  (with-current-buffer (get-buffer-create " *fontify buffer*")
    (erase-buffer)
    (lisp-mode)
    (insert string)
    (font-lock-fontify-buffer)
    (buffer-string)
    ))
(progn
  (lisp-fontify-string "(defun foo (x) (* x x))")
  (pop-to-buffer (get-buffer-create " *fontify buffer*")))

(upcase "λ")                           ; "Λ"

(defun self-insert-command-invert (n)
  (interactive "p")
  (setq #1=last-command-char (upcase #1#))
  (self-insert-command n))

(defun invert-case (c)
  "文字を反転する."
  (if (eql (upcase c) c)
      (downcase c)
      (upcase c)))

;; @@SKK
;; skk-init-file をバイトコンパイルする
(setq skk-byte-compile-init-file t)

;; xclip.el
;; http://www.mail-archive.com/gnu-emacs-sources@gnu.org/msg01336/xclip.el

;; どこかで役に立つかもしれない呪文
(add-to-list 'mime-charset-coding-system-alist '(shift_jis . cp932))

;; @@time&date
;; 日付オブジェクト
(current-time)                          ; (19314 37173 747025)
(nth 4 (file-attributes "memo.el"))     ; (19314 38873)
(float-time)
;; encode
(encode-time 0 0 9 1 1 1970)            ; (0 0)
;; decode
(decode-time '(0 0))                    ; (0 0 9 1 1 1970 4 nil 32400)
(decode-time '(0 1))                    ; (1 0 9 1 1 1970 4 nil 32400)
(decode-time '(1 0))                    ; (16 12 3 2 1 1970 5 nil 32400)
;; format
(format-time-string "%Y-%m-%dT%H:%M:%S" '(0 0)) ; "1970-01-01T09:00:00"
;; parse
(parse-time-string "1970-01-01T09:00:00")
(parse-time-string (current-time-string)) ; ~= (decode-time (current-time))
(current-time-string '(0 0))            ; "Thu Jan  1 09:00:00 1970"

;; ISO 8601 format
(format-time-string "%Y-%m-%dT%T%z")    ; "2009-04-14T04:44:54+0900"

;; UNIX time
(decode-time '(0 0 0))                  ; (0 0 9 1 1 1970 4 nil 32400)
;; Universal Time [CL]
(multiple-value-list
 (decode-universal-time 0))             ; (0 0 9 1 1 1900 0 NIL -9)

;; From: $XYZZYHOME/lisp/imagehdr.l
(defun read-png-header (filename)
  (with-temp-buffer
    (insert-file-contents filename nil 0 24)
    (when (looking-at "\x89PNG\r\n\x1a\n")
      (labels ((%read-char ()
                 (forward-char 1)
                 (following-char)))
        (goto-char 16)
        (list #1=(+ (ash (%read-char) 24)
                    (ash (%read-char) 18)
                    (ash (%read-char) 8)
                    (%read-char))
              #1#
              "PNG")))))
(read-png-header "~/Pictures/bknr_lisp.png")
(read-png-header "memo.el")

(defun asciidoc-buffer ()
  (interactive)
  (let ((filename buffer-file-name))
    (unless filename
      (error "ファイルを保存してください"))
    (shell-command (format "asciidoc %s" filename))))
(defun asciidoc-preview ()
  (interactive)
  (asciidoc-buffer)
  (browse-url (browse-url-file-url
               (format "%s.html" (file-name-sans-extension
                                  (buffer-file-name))))))

(defun make-list-from-keyword-table (hash)
  (let (acc)
    (when (hash-table-p hash)
      (maphash #'(lambda (x y) (push (cons x y) acc)) hash))
    acc))
(fset 'hash->list #'make-list-from-keyword-table)

(defun nr-to-char-region (from to)
  "HTMLの数値参照を実体文字に変換する."
  (interactive "*r")
  (save-excursion
    (while (re-search-forward "&#x?\\([[:xdigit:]]+\\);" nil t)
      (if (string= (substring (match-string 0) 0 3) "&#x")
          (replace-match (string (unicode-char (string-to-number (match-string 1) 16))))
          (replace-match (string (string-to-number (match-string 1) 10)))))))

(char-charset ?\あ)                     ; japanese-jisx0208
;; 文字コード一覧
(coding-system-list)
coding-system-alist
(let (acc)
  (do-symbols (s)
    (and (coding-system-p s)
         (not (string-match "\\(unix\\|dos\\|mac\\)$" (symbol-name s)))
         (push s acc)))
  (nreverse acc))

;; HTML 4.01仕様書を読む
;; http://www.ne.jp/asahi/alpha/kazu/tips.html#html-spec
;; HTML 4.01 Specification - http://www.w3.org/TR/html401/

;; PHPdoc.el
;; PHP - http://www.ne.jp/asahi/alpha/kazu/php.html#phpdoc

(defun sgml-char-to-cref (x)
  (if (stringp x)
      (setq x (string-to-char x)))
  (cdr (assq x '((?& . "&amp;")
                 (?< . "&lt;")
                 (?> . "&gt;")
                 (?\" . "&quot;")
                 (?' . "&apos;")
                 (?  . "&nbsp;")        ; 2208
                 ))
       ))
(defun sgml-char-to-nref (x &optional base)
  (if (stringp x)
      (setq x (string-to-char x)))
  (if (eql base 16)
      (format "&#x%x;" (char-unicode x))
      (format "&#%d;" (char-unicode x))))
(sgml-char-to-cref (unicode-char #x00a0))
(sgml-char-to-nref (unicode-char #x00a0) 16)

;; nobreak-space (U+00A0) をハイライトする (デフォルトで t)
(setq nobreak-char-display t)

;; Unicode文字(U+XXXX)を入力する
(insert #x00a0)
(insert-char #x00a0)                    ; C-x 8 RET

;; tcl-mode
;; http://www.opensource.apple.com/source/gcc3/gcc3-1161/dejagnu/tcl-mode.el

;; isearch
;; C-s RETと入力することで, 非インクリメンタルサーチができます.

;; 二重置換が起こる場合があるのでお蔵入り -> "&amp;lt;"
(defun sgml-unquote-region (start end)
  "リージョン内の文字実体参照を元の文字に変換する."
  (interactive "*r")
  (labels ((rep (re-from to)
             (save-excursion
               (goto-char (point-min))
               (while (re-search-forward re-from nil t)
                 (replace-match to nil t)))))
    (save-restriction
      (narrow-to-region start end)
      (rep "&nbsp;" "\u00A0")           ; U+00A0
      (rep "&lt;" "<")
      (rep "&gt;" ">")
      (rep "&quot;" "\"")
      (rep "&amp;" "&")
      )))

;; Cygwinとのプロセス通信は改行コードを変更した方がいいかもしれない
(setq default-process-coding-system '(utf-8-dos . utf-8-unix))

$ emacs --batch --eval '(progn (find-file "ln-bash") (print (buffer-string)))'
Loading subst-ksc...
Loading subst-gb2312...
Loading subst-big5...
Loading subst-jis...

"!<symlink>/bin/bash.exe\000"

;; ファイル内のエンコーディング文字を無視する
(delete 'sgml-html-meta-auto-coding-function auto-coding-functions)

;; ピコピコ
;; MML (Music Macro Language) 編集モード
;; http://www.iil-inc.com/~wabee/music/zmusic.html#zmusic-mode
"http://www.iil-inc.com/~wabee/music/zmusic-mode.el"

;; ピコカキコがしたい
(defun mml-play (mml)
  (let* ((flashplayer (case system-type
                        (windows-nt "c:/usr/local/flash/Players/FlashPlayer.exe")
                        (t "/usr/local/bin/flashplayer")))
         (swf (case system-type
                (windows-nt "c:/home/niconico/mml/flmml.swf")
                (t "/windows/home/niconico/mml/flmml.swf")))
         (url (format "file://%s?mml=%s"
                      (expand-file-name swf)
                      (si:www-url-encode mml nil "-A-Za-z0-9_.!'(|)"))))
    ;(browse-url-default-browser url)
    (start-process-shell-command "FLMML" nil flashplayer url)
    ))
;"[:alnum:]"
(defun mml-play-file (filename)
  "MMLファイルの中身をURL形式でFLMMLに渡して再生する."
  (interactive (list (read-file-name "MML file: " (get-buffer-file-name))))
  (mml-play (with-temp-buffer
              (insert-file-contents filename)
              (buffer-string))))
;;(mml-play "T120O6L16V12 @1@E1,0,10,0,0 /:4 AR>AR<ERARA>AR<ERRAR :/; O6L16V5 @1@E1,0,10,0,0 R8. /:4 AR>AR<ERARA>AR<ERRAR :/; O3L8V15 @0@E1,2,30,30,8 AAAAAAAA AAAAAAAA<DDDDEEEE>AAAAAAAA; L8V12 /:8 @4@E1,0,6,0,0 CC @E1,0,15,0,0C @E1,0,6,0,0 C:/")
;;(mml-play-file "http://dic.nicovideo.jp/mml/3575")

;; コメントの文法が同じなので
(add-to-list 'auto-mode-alist '("\\.fml" . c-mode))
;(add-to-list 'auto-mode-alist '("\\.mml" . c-mode))

(define-generic-mode fml-mode
    ;; comment
    '(("/*" . "*/") ("/:0 " . " :/"))
  ;; keyword
  nil
  ;; font-lock-list
  nil
  ;; aut-mode-list
  '("\\.fml\\'")
  ;; hook
  '(generic-bracket-support)
  "Generic mode for Samba configuration files.")

;; defvaralias はemacs21には存在しないので defalias で代用できる?

;;; 現在のバッファを見ながらシェル
(defadvice shell (around shell-split-windows activate)
  (let ((shell-buffer (save-window-excursion ad-do-it)))
    (or (eq shell-buffer (current-buffer))
        (pop-to-buffer shell-buffer 'other-window))))

(define-key mode-specific-map [?s] 'shell)

(fset 'basename #'file-name-nondirectory)
(fset 'dirname #'file-name-directory)

(window-list)
(defun visible-buffer-list (&optional minibuf all-frames)
  (let (bufs)
    (walk-windows #'(lambda (window)
                      (push (window-buffer window) bufs))
                  minibuf all-frames)
    (nreverse bufs)))

;; ?
(set-selection-coding-system 'compound-text-with-extensions)

;; 自作or手動インストールライブラリはどこに置いておくのが定石？
root
====
/usr/share/emacs/site-lisp/
/usr/local/share/emacs/site-lisp/

user
====
~/share/emacs/site-lisp/
~/emacs.d/
~/elisp/
~/lib/emacs/

(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (normal-top-level-add-subdirs-to-load-path))

;; トグルキーを作る
(define-prefix-command 'toggle-sense-map)
(global-set-key "\C-xt" 'toggle-sense-map)

(define-key toggle-sense-map "e" 'toggle-debug-on-error)
(define-key toggle-sense-map "f" 'toggle-truncate-lines)
(define-key toggle-sense-map "s" 'toggle-scratch-buffer)

;; Elispの仕様変更などの記述はどの辺を参照すればいいの？
/usr/local/src/emacs*/lisp/ChangeLog*

;; $ echo "こんにちは" | nkf -g
(let ((process-connection-type nil)
      (proc (start-process "nkf" nil "nkf" "-g")))
  (set-process-filter proc #'(lambda (p s) (message "%s" s)))

  (process-send-string proc "こんにちは")
  (sleep-for 0.1)
  (process-send-eof proc)
  ;(process-describe proc)
  )

(defun url-decode-region (start end)
  (interactive "*r")
  (prog1
      (insert (si:www-url-decode
               (buffer-substring-no-properties start end)))
    (delete-region start end)))

;; tramp/angle-ftp
;; executable-make-buffer-file-executable-if-script-pにて
;; file-modesがリモートファイルに対応してないのでエラーになる件
;; set-file-modesも同様と思われる
(defadvice executable-make-buffer-file-executable-if-script-p
    (around ignore-if-ftp activate)
  (or (string-match "ftp:" (buffer-file-name))
      ad-do-it))

;; @@mew
(eval-after-load "mew"
  (quote
   (progn
     (define-key mew-summary-mode-map "g" 'mew-status-update))))

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

(load "site-lisp/unicode-escape")
(unicode-unescape (unicode-escape "\u685c\u6728\u674e\u674f"))

;; いつの間にかマニュアルが文字化けしていた (2009-11-09)
(if (eq system-type 'windows-nt)
    (defadvice man (around manpage-ja activate)
      (let ((locale-coding-system 'euc-jp-unix))
        ad-do-it)))

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

;;-> sgml-quote で代用可能

;; yank時にプロパティを取り除く
(setq yank-excluded-properties t)

;; モードラインの改行コード表示
;; デフォルト
(setq eol-mnemonic-dos "\\"
      eol-mnemonic-unix "(Unix)"
      eol-mnemonic-mac "(Mac)"
      eol-mnemonic-undecided ":")
(setq eol-mnemonic-dos "d"
      eol-mnemonic-unix "u"
      eol-mnemonic-mac "m"
      eol-mnemonic-undecided "?")
(setq eol-mnemonic-dos "crlf"
      eol-mnemonic-unix "lf"
      eol-mnemonic-mac "cr"
      eol-mnemonic-undecided "?")

(defsetf lookup-key (keymap key) (def)
  `(define-key ,keymap ,key ,def))
(setf (lookup-key minibuffer-local-map (kbd "C-w")) 'delete-backward-word)

;; Emacs Part 48
;; 55: これやったら標準の補完がちょっとリッチになった
(add-hook 'minibuffer-setup-hook 
          (lambda () 
            (when (cl-loop with c = (current-local-map) 
                           for m = c then (keymap-parent m) 
                           while m 
                           when (eq m minibuffer-local-completion-map) 
                           return t 
                           finally return nil) 
              (add-hook 'post-command-hook 
                        (lambda () 
                          (cl-letf (((symbol-function 'message) 
                                     'ignore)) 
                            (minibuffer-completion-help))) 
                        nil t))))

;; 2015-05-13
;; NTEmacs がバッファ切り替え/ファイル読み書き時にプチフリーズする問題(10秒程度)
;; 設定ファイルを読み込まない状態(-Q)でも同様
;; エラーメッセージは特にないため原因がどこにあるのか分からない...

;; ファイルの書き込みのタイミングでNTEmacsがフリーズする？
;; http://ksugita.blog62.fc2.com/blog-entry-11.html

(setq make-backup-files nil
      version-control nil)
(add-to-list 'auto-save-file-name-transforms
             (list ".*" temporary-file-directory t))
;; ~/.emacs.d/auto-save-list/
'(setq auto-save-list-file-name nil
      auto-save-list-file-prefix nil)

;; カーソルの点滅とIME変換の相性？
;; http://osdn.jp/projects/gnupack/forums/22824/33700/
;; http://d.hatena.ne.jp/ksugita0510/20130316/p1
(setq ime-enable-reconversion nil
      ime-enable-document-feed nil)

;; eww
(setq-default eww-search-prefix "https://www.google.co.jp/search?q=")
;; `shr-render-buffer' でHTMLファイルをレンダリングすると面白い

;; [d] `eww-download'  はヘッダー部分を除去しないバグがあるため使用に注意 (2015-09-09)

;; gdb
(custom-set-variables
 '(gdb-many-windows t)                  ;情報表示
 '(gud-tooltip-echo-area t)             ;mini bufferに値を表示
 '(gud-tooltip-mode t)                  ;ポップアップで情報
 )

(mapc #'funcall kill-emacs-hook)

;; desktop-save に循環リスト入り変数を食わせると無限再帰でemacsが終了しない (or メモリを食い荒らす)
;; -> print-circle が原因？
(setf search-ring '("first" "second" "third"))
(setf (cdr (last search-ring)) search-ring)

(setf command-history nil)
;; M-x eval-expression #1=[#1#]
command-history                   ;=> ((eval-expression [#2] nil))
(desktop-outvar 'command-history) ;-> (error "Lisp nesting exceeds `max-lisp-eval-depth'")

;; ラベルが参照するS式に自分自身が含まれると再帰的に評価される。無限ループはバグというより仕様？
(identity #1=(vector #1#)) ;-> (error "Lisp nesting exceeds `max-lisp-eval-depth'")
(identity #1=[#1#])        ;=> [#0]

;; プロパティ付き文字列？
#("/ssh:localhost:~/.bashrc" 1 6 (tramp-default t))
(type-of #("HELLO"))                    ;=> string

;; `plist-put' はリストを破壊的に更新するが、
;; 元の値がnilの場合のみ値が反映されない。
;; -> 必ず (setq x (plist-put x ...)) か (setf (cl-getf x ...) ..) を利用すること
(let ((plist-el '())
      (plist-cl '()))
  (plist-put plist-el :key 10)
  (setf (cl-getf plist-cl :key) 10)
  (list plist-el plist-cl))
;=> (nil (:key 10))

;; NTEmacs+Cygwin/aspell の組み合わせにてプロセス起動時に `NUL' ファイルが作成される
;; > cygwin\bin\rm.exe NUL で削除する

(defun user:find-all (re obj)
  (let (acc)
    (with-temp-buffer
      (insert (cl-typecase obj
                (buffer (with-current-buffer obj (buffer-string)))
                (string obj)
                (otherwise (error "buffer or string expected"))))
      (goto-char (point-min))
      (while (re-search-forward re nil t)
        (push (match-string-no-properties 0) acc)))
    (nreverse acc)))

(user:find-all cfirchat-url-regexp (current-buffer))

;; 便利だが package-install 時に誤爆してコケるので却下
(defun in-load-path-p (dir)
  (cl-some #'(lambda (path)
               (sub-directory-p dir path))
           load-path))

(defvar auto-view-mode-directories load-path)

(defun auto-view-mode ()
  "指定されたディレクト以下のファイルを`view-mode'で開きます."
  (let ((dir (file-name-directory (buffer-file-name))))
    (if (cl-some #'(lambda (path)
                     (sub-directory-p dir path))
                 auto-view-mode-directories)
        (view-mode))))

(add-hook 'find-file-hook 'auto-view-mode)
(remove-hook 'find-file-hook 'auto-view-mode)

;; SUB-FEATURE とは?
(featurep 'make-network-process '(:family local))

;; パッケージからインストールしたものを列挙する
(let (acc)
  (mapatoms (lambda (s)
              (let* ((source (ignore-errors (feature-file s))))
                (when (and (stringp source)
                           (sub-directory-p source package-user-dir))
                  (pushnew (file-name-base source) acc :test #'equal)))))
  (nreverse acc))
;;=> ("anzu" "paredit-autoloads" "request-deferred" "elisp-slime-nav")

;; コマンドライン文字列のテンプレート
(require 'format-spec)

(let ((host "localhost")
      (port "https"))
  (format-spec
   "gnutls-cli --insecure -p %p %h"
   `((?h . ,host)
     (?p . ,(if (integerp port) (int-to-string port) port)))))
;;=> "gnutls-cli --insecure -p https localhost"

;; [DEPRECATED] https://github.com/joaotavora/sly-company
(use-package sly-comapny
  :ensure company
  :config
  (add-hook 'sly-mode-hook 'sly-company-mode)
  (add-to-list 'company-backends 'sly-company))

;; TODO: highlight
(defun highlight-todos ()
  (highlight-phrase
   (rx word-start (or "BUG" "FIXME" "TODO" "NOTE") ":")))
(add-hook 'find-file-hook 'highlight-todos)

;; FIXME: emacs-lisp 編集時にカーソルが括弧の先頭にある場合
;; シンボル補完や Xref の候補が「関数のみ」に限定されてしまうのが不便。変数も検索したい

;; https://github.com/mnewt/dotemacs/blob/be862d6a6fba35102a7feca8fb8a8f5e634c6892/init.el#L909-L916
(defun edebug-mode-info (_symbol newval _operation _where)
  "Display an indicator when `edebug' is active.

Watches `edebug-active' and sets the mode-line when it changes."
  (setf (alist-get 'edebug-mode mode-line-misc-info)
        (list (when newval "ED"))))
(add-variable-watcher 'edebug-active #'edebug-mode-info)

;; All those have been changed for Emacs 28
(setq xref-show-definitions-function #'xref-show-definitions-completing-read) ; for M-.
(setq xref-show-xrefs-function #'xref-show-definitions-buffer) ; for grep and the like
(setq xref-file-name-display 'project-relative)
(setq xref-search-program 'grep)

;;; マウス関連 mouse

;; スクロール時のフォント再表示を抑制 (?)
(setq  redisplay-skip-fontification-on-input t
       fast-but-imprecise-scrolling t)
;; 入力時にマウスカーソルを隠す？
(setq make-pointer-invisible nil)
