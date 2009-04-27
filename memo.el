;;; -*- mode: Lisp-Interaction -*-
;;; Emacs専用メモ

;;; 文字(キー)の表現方法色々
;;; *** 基本はベクタ
?h                                      ; 104
[?h]                                    ; [104]
?\C-h                                   ; 8
"\C-h"                                  ; ""
[(f9)]                                  ; F9
(coerce "\C-h" 'vector)                 ; [8] (vconcat "\C-h")
(global-set-key [(control ?c) (control ?v)] 'ignore)
(lookup-key global-map [(control ?c) (control ?v)]) ; ignore
(kbd "<up> <up> <down> <down> <left> <right> <left> <right> b a")
;; [up up down down left right left right 98 97]
(kbd "C-m")                             ; ""
[return]                                ; "\C-m"
(kbd "C-M-<up>")                        ; [C-M-up]
[(control meta up)]
(equal "\M-\C-e" "\C-\M-e")             ; t
(kbd "C-M-d")                           ; [134217732]
(kbd "C-.")                             ; [67108910]

(every #'char-valid-p '(0 1 2 254 255)) ; t

;;; Ctrl-H を前1文字削除に変更
(keyboard-translate ?\^h ?\177)
(keyboard-translate ?\^h 'backspace)
(keyboard-translate ?\C-h ?\C-?)
(load-library "keyswap")

;;; C-h と BS が同じ挙動になる
(global-set-key [backspace] 'backward-delete-char)
(keyboard-translate ?\C-h 'backspace)
(global-set-key [delete] 'delete-char)

backward-delete-char-untabify-method    ; untabify/hungry/all/nil
(define-key global-map "" 'backward-delete-char-untabify) ; == "\C-h"

;; isearch
(define-key isearch-mode-map "\C-k" 'isearch-edit-string)

;; ls の出力を英語にする "$ LANG=C ls"
;; <on cygwin> 実行後も LANG が変更されたまま
'(add-hook 'dired-mode-hook
           '(lambda ()
              (make-local-variable 'process-environment)
              (setenv "LANG" "C")))

;;; EmacsHelpの呼び出し (help-commandはF1からも呼び出せる)
(global-set-key "\C-c\C-h" 'help-command)
(global-set-key "\M-?" 'help-command)

;;; fill
(setq paragraph-start '"^\\([ 　・○<\t\n\f]\\|(?[0-9a-zA-Z]+)\\)")

;;; 端末から起動した時 (emacs -nw) にメニューバーを消す
(if window-system (menu-bar-mode 1) (menu-bar-mode -1))

;;; 全角空白とか改行とかタブを強調表示
(require 'jaspace)
(autoload 'jaspace-mode-on "jaspace" nil t)
(setq jaspace-mode '(c-mode))
(setq jaspace-alternate-jaspace-string "□")
(if window-system
    (setq jaspace-alternate-eol-string "\xab\n"))
(setq jaspace-highlight-tabs ?^)	; use ^ as a tab marker

;;; 行末の無駄な空白文字を表示
(setq-default show-trailing-whitespace t)
(set-face-background 'trailing-whitespace "plum") ;;"SteelBlue")
;; (set-face-underline 'trailing-whitespace t)
(defun toggle-trailing-whitespace (&optional arg)
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace))
  (font-lock-mode t))
(defun turn-off-show-trailing-whitespace ()
  (interactive)
  (setq show-trailing-whitespace nil))
(add-hook 'shell-mode-hook 'turn-off-show-trailing-whitespace)

;; mew から拝借した(らしい)
(defvar cfirchat-url-regexp
  "\\b\\(s?https?\\|ftp\\|file\\|gopher\\|news\\|telnet\\|wais\\|mailto\\):\\(//[-a-zA-Z0-9_.]+:[0-9]*\\)?[-a-zA-Z0-9_=?#$@~`%&*+|\\/.,]*[-a-zA-Z0-9_=#$@~`%&*+|\\/]"
  "* URL にマッチする正規表現")

;; カーソル位置のfaceを調べる関数
(defun describe-face-at-point()
  "Return facce used at point."
  (interactive)
  (message "%s" (get-char-property (point) 'face)))

(global-set-key "\M-g" 'goto-line)

;;; shell-mode ではパスワードを表示しない
;;; 多分デフォルト
(add-hook 'comint-output-filter-functions
	  'comint-watch-for-password-prompt)

;;; リドゥ (アンドゥのアンドゥ)
;;; http://www.fan.gr.jp/~ring/Meadow/elisp/redo.el
(require 'redo)
(global-set-key [?\C-_] 'redo)

;;; マウスに反応
(define-key global-map [mouse-4] 'scroll-down)
(define-key global-map [mouse-5] 'scroll-up)
(mwheel-install)
(setq mouse-wheel-follow-mouse t)

;; バックアップファイルの保存場所を指定
;; ~/emacs_backup ディレクトリに保存
(setq make-backup-files t)
(setq backup-directory-alist
      (cons (cons "\\.*$" (expand-file-name "~/emacs_backup"))
            backup-directory-alist))

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

;;; カーネル編集用 C-mode
(defun linux-c-mode ()
  "C mode with adjusted defaults for use with the Linux kernel"
  (interactive)
  (c-mode)
  (setq c-indent-level 8)
  (setq c-brace-imaginary-offset 0)
  (setq c-brace-offset -8)
  (setq c-argdecl-indent 8)
  (setq c-label-offset -8)
  (setq c-continued-statement-offset 8)
  (setq indent-tabs-mode nil)
  (setq tab-width 8))
(setq auto-mode-alist (cons '("/usr/src/linux.*/.*\\.[ch]$" . linux-c-mode)
                            auto-mode-alist))

;;; ガーベジコレクトの回数を減らす (デフォは 4000000)
(setq gc-cons-threshold 5000000)

;;; mode-info --- 関数・変数の説明文を参照するコマンド
;;; Emacs lisp / C / Perl / Ruby のマニュアルを便利に
;;; http://namazu.org/~tsuchiya/elisp/mode-info.html

;;; モード名をもっと短く
(add-hook 'emacs-lisp-mode-hook
          #'(lambda () (setq mode-name "Elisp")))

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

;;; 現在の関数名をモードラインに表示
(which-function-mode t)

;;; M-x: cusomize-apropos

;;; 同一ファイル名のバッファ名を分かりやすく
;;; 片方のバッファを消すとき無駄なバッファが残る
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
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
;;;;;  </lisp/man.el>関数 Man-getpage-in-background 744行目あたり
;; ;; (let (...
;; ;;       (coding-system-for-read
;; ;;        (if default-enable-multibyte-characters
;; ;; 	         locale-coding-system 'raw-text-unix))
;; ;;       ...))
;;; うちの環境は locale-coding-system => cp392 だった
;; (defadvice man (around man-pages-ja activate)
;;   (let ((locale-coding-system
;; 	 (read-coding-system
;; 	  (format "Coding (default %s): " 'japanese-iso-8bit)
;; 	  'japanese-iso-8bit)))
;;     ad-do-it))

;;; よそのウィンドウにカーソルを表示しない
;;; 他のライブラリが上書きしてるかも
(setq cursor-in-non-selected-windows nil)

;;; マウスの位置でなく、カーソルの位置にペーストする
(setq mouse-yank-at-point t)

;; Shift+カーソルでリージョン選択
(pc-selection-mode)

;;; (Meadowではエラー) setq: Spawning child process: exec format error
;; (setq exec-suffixes '(".exe" ".sh" ".pl"))

;; クオートでコメントアウトしたS式はコメントの色にする
(defun elisp-font-lock-top-quote (limit)
  (when (re-search-forward "^' *(" limit t)
    (forward-char -1)
    (set-match-data (list (point) (progn (forward-sexp 1) (point))))
    t))
(font-lock-add-keywords
 'emacs-lisp-mode
 '((elisp-font-lock-top-quote 0 font-lock-comment-face prepend)))

;; http://www.emacswiki.org/cgi-bin/emacs/download/blank-mode.el
;; http://www.emacswiki.org/elisp/show-wspace.el

;; 無限ループになるかもしれないよ
(defun kitaa ()
  (interactive)
  (let ((kita-list '("ﾟ∀ﾟ" " ﾟ∀" "   ﾟ" "    " "ﾟ   " "∀ﾟ " "ﾟ∀ﾟ")))
    (while t
      (dolist (kao kita-list)
        (message "ｷﾀ━━━(%s)━━━!!!!" kao)
        (sit-for .1)))))

(system-name)                           ; "YOUR-D1BE424ADF"
system-time-locale                      ; nil
system-messages-locale                  ; nil
system-configuration                    ; "i386-mingw-nt5.1.2600"
system-type                             ; windows-nt
temporary-file-directory                ; "c:/tmp/"

(split-string (getenv "PATH") ";")      ; == exec-path

(defun windows-p ()
  (if (memq system-type '(ms-dos windows-nt)) t nil))

;; etags 使ったことない
(defun make-tags-file (dir)
  (interactive "DMake TAGS file: ")
  (declare (ignore dir))
  (shell-command (format ;; "%s *.[ch] *.el --output=TAGS"
                  "%s *.el --output=TAGS"
                  (expand-file-name "etags.exe" exec-directory))))

;; `;' とか `{' を入力するとと自動的に改行される
(c-toggle-auto-newline)                 ; C-c C-a

(key-description "\C-x \M-y \n \t \r \f123")
=>"C-x SPC M-y SPC C-j SPC TAB SPC RET SPC C-l 1 2 3"

(mapcar (lambda (x)
          (list (text-char-description x) (string x)))
        '(?\C-c ?\M-m ?\C-\M-m))
=>(("^C" "") ("\xed" "\355") ("\x8d" "\x8d"))

(getenv "programfiles")                 ; "C:\\Program Files"

;; 
(defun run-slime (&optional modern)
  (interactive "P")
  (shell-command
   (format "%s -K full -q -ansi %s -i %s &"
           (expand-file-name "bin/clisp.exe" clisp-directory)
           ;; inferior-lisp-program
           (if modern "-modern" "")
           (expand-file-name ".slime.lisp" clisp-directory)))
  (sleep-for 0.5)
  (slime-connect "127.0.0.1" 4005))

;;; CLISPごった煮2.46の設定
(progn
(add-to-list 'load-path "c:/usr/local/clisp-2.46-full/lib/slime")
(load-library "slime")
(slime-setup '(slime-scratch slime-fancy inferior-slime))
(setq slime-net-coding-system 'utf-8-unix) ; 日本語を使いたい
(setq slime-lisp-implementations
      '((clisp ("C:/usr/local/clisp-2.44/clisp.exe" "-K full"
                "-Efile utf-8" "-on-error debug" "-I"))
        (olio ("c:/usr/local/clisp-2.46-full/bin/clisp.exe")
         :init;; slime-olio-init-command
         slime-init-command
         )))
(setq slime-default-lisp 'olio)
(global-set-key [(control ?c) (control ?z)] 'slime-repl)
(define-key slime-repl-mode-map [(control ?c) (control ?c)] 'slime-quit-lisp)
(require 'hyperspec)
(global-set-key "\C-cH" 'hyperspec-lookup)
;; CLISP ごった煮
(load-library "clisp-olio")
)

;;; SLIME Tips
;;; http://www.cliki.net/SLIME%20Tips
;; Connecting automatically
(defun cliki:start-slime ()
  (unless (slime-connected-p)
    (save-excursion (slime))))
(add-hook 'slime-mode-hook 'cliki:start-slime)

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

;; ラムダ式の比較
(equal #'(lambda (x) (+ x x))
       #'(lambda (x) (+ x x)))          ; t


;;; @@Encoding

;;; 非ASCII文字 -- GNU Emacs Lispリファレンスマニュアル
;;; http://www.bookshelf.jp/texi/elisp-manual/21-2-8/jp/elisp_33.html

;; エンコード関数・変数 たぶんまだある
;; mule{,cmds}.el
mule-keymap
universal-coding-system-argument (C-x RET c)
set-buffer-file-coding-system    (C-x RET f)
set-file-name-coding-system      (C-x RET F)
set-buffer-process-coding-system (C-x RET p)
revert-buffer-with-coding-system (C-x RET r)
set-terminal-coding-system       (C-x RET t)
set-keyboard-coding-system       (C-x RET k)

(list coding-system-for-read coding-system-for-write) ; (nil nil)
locale-coding-system                                  ; cp932

(apropos "default-.*-coding-system")
default-buffer-file-coding-system                     ; japanese-shift-jis
default-file-name-coding-system                       ; japanese-shift-jis
default-keyboard-coding-system                        ; japanese-shift-jis
default-process-coding-system                         ; (japanese-shift-jis-dos . japanese-shift-jis-unix)
default-sendmail-coding-system                        ; iso-2022-jp
default-terminal-coding-system                        ; japanese-shift-jis
(default-value 'buffer-file-coding-system)            ; japanese-shift-jis

(coding-system-base 'utf-8)                           ; mule-utf-8
(detect-coding-string "縺薙�ｰ繧�縺�")                 ; (iso-latin-1 emacs-mule raw-text no-conversion)
(detect-coding-string (encode-coding-string "こばやし" 'utf-8)) ; (japanese-shift-jis mule-utf-8 raw-text no-conversion)
(detect-coding-string "こばやし") ; (japanese-shift-jis iso-latin-1 emacs-mule raw-text no-conversion)
(find-coding-systems-string "こばやし")
(find-charset-string "abcほげ")         ; (ascii japanese-jisx0208)
(find-charset-string "abc")             ; (ascii)

(let ((encode (encode-coding-string "こばやし" 'utf-8)))
  (list encode (decode-coding-string encode 'utf-8))) ; ("\343\201\223\343\201\260\343\202\204\343\201\227" "こばやし")

(map-internal-to-utf-8 "こばやし") ; "縺薙�ｰ繧�縺�" @xyzzy
(encode-coding-string "こばやし" 'utf-8) ; "\343\201\223\343\201\260\343\202\204\343\201\227" @emacs
(string-as-multibyte (string-as-unibyte "こばやし"))        ; "こばやし"
(unibyte-char-to-multibyte (multibyte-char-to-unibyte ?あ)) ; 2210 (あれ？) 
(apply #'make-char (split-char ?あ))    ; 53794 (#o151042, #xd222, ?あ)

;; 文字列からcharsetの判定方法は？ (intern string) でなくて
coding-system-alist

(coding-system-list 'base-only)
charset-list
last-coding-system-used

(detect-coding-with-language-environment (point-min)
                                         (min (point-max) #x1000)
                                         "Japanese")
;; (iso-latin-1-unix emacs-mule-unix raw-text-unix no-conversion)
(detect-coding-region (point-min) (min (point-max) #x1000))
;; (iso-latin-1-unix emacs-mule-unix raw-text-unix no-conversion)

;; (set-language-environment "Japanese")
;; current-language-environment
;; (set-default-coding-systems 'japanese-shift-jis-dos)
;; (set-clipboard-coding-system 'japanese-shift-jis-dos)
;; (set-w32-system-coding-system 'japanese-shift-jis-dos)
;; (setq default-file-name-coding-system 'japanese-shift-jis)
;; (setq-default buffer-file-coding-system 'japanese-shift-jis-dos)
;; (setq default-terminal-coding-system 'japanese-shift-jis-dos)
;; (setq default-keyboard-coding-system 'japanese-shift-jis)
;; (setq default-process-coding-system '(japanese-iso-8bit . japanese-iso-8bit))


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

;; こいつはレキシカルだ
(lexical-let ((count 0))
  (defun counter ()
    (incf count)))
(list (counter) (counter))              ; (1 2)

;; n日後を返す関数を返す関数 (どう書く？org)
;; http://ja.doukaku.org/comment/1273/
(defun make-ndays-later (n)
  (lexical-let ((n n))
    (lambda (time)
      (apply (lambda (s mi h d m y dow dst zone)
               (encode-time s mi h (+ d n) m y))
             (decode-time time)))))
(fset 'five-days-later (make-ndays-later 10))
(format-time-string "%Y/%m/%d %T" (five-days-later (current-time)))
;;=> "2008/12/25 06:02:02"

;;; elisp/CL の違い
;; 102: Emacs Lisp と Common Lisp は似ているのですか?
;; http://stuff.mit.edu/afs/athena/astaff/project/babel/build/sun4/etc/FAQ.jp
;; 大文字小文字を区別する
;; 動的スコープ
;; パッケージが無い
;; 多値が無い
;; リーダマクロが無い
;; 有理数、不動小数、多倍長整数が無い

;; http://cl-cookbook.sourceforge.net/.emacs
;; NTEmacs だと中途半端に拡大されるんだが
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
    (set-frame-position frame 0 0)))

;;; @@window
(window-inside-edges)                   ; (1 1 123 20)

;; マウスポジション
;; フレームを外れると nil になるらしい
(list (cdr (mouse-pixel-position))
      (cdr (mouse-position)))           ; ((587 . 269) (73 . 16))

;; ウィンドウポジション、高さと幅
(list (cons (x-display-pixel-height) (x-display-pixel-width))
      (cons (x-display-mm-height) (x-display-mm-width))
      (posn-x-y (posn-at-point))
      (cons (window-height) (window-width)))

;; (window area-or-pos (x . y) timestamp object pos (col . row) image (dx . dy) (width . height))
(destructuring-bind (0posn-window
                     1posn-area
                     2posn-x-y
                     3posn-timestamp
                     4posn-string
                     5posn-point
                     6posn-actual-col-row
                     7posn-image
                     8posn-object-x-y
                     9posn-object-width-height)
    (posn-at-point) )

;; フレームポジション
(list (cons (frame-pixel-height) (frame-pixel-width))
      (cons (frame-height) (frame-width))
      (cons (frame-char-height) (frame-char-width)))

;; カーソルポジション
(list (posn-col-row (posn-at-point))
      (posn-actual-col-row (posn-at-point))
      (cons (window-start) (window-end))
      (cons (point-min) (point-max)))

(current-frame-configuration)
(frame-parameters)
(current-window-configuration)

;; @@face/color
(list-colors-display)

(require 'linum)
(linum-on)

(defadvice indent-sexp (around indent-defun (&optional endpos))
  "Indent the enclosing defun (or top-level sexp)."
  (interactive)
  (save-excursion
    (beginning-of-defun)
    ad-do-it))

;; subr.el
(number-sequence 0 3 .5)                ; (0 0.5 1.0 1.5 2.0 2.5 3.0)

;; keymap が作られる前だと define-key ができなくてエラー
(define-key hoge-map [up] 'hoge-hoge)
;; 対処法
(add-hook 'hoge-mode-hook
          #'(lambda ()
              (define-key hoge-map [up] 'hoge-hoge)))
(eval-after-load "hoge"
  '(define-key hoge-map [up] 'hoge-hoge))

;; Blogger 関連
;; atom-api (+ nxml)
;; make が失敗するので手動で
;; $ emacs -batch -q -no-site-file -l rng-auto.el -f rng-byte-compile-load
(load "c:/tmp/nxml-mode-20041004/rng-auto")

;; $ emacs -Q --batch --eval '(print (upcase-initials "LL day and night"))'

(expand-file-name "~/Desktop.lnk")      ; "c:/home/lxuser/Desktop.lnk"
(file-truename "~/Desktop.lnk") ; "c:/Documents and Settings/shigeru/デスクトップ"

;; :key使いづらくないか？
(funcall (lambda (&key absolute) (list absolute)) :absolute t) ; (t)
(funcall (lambda (&key absolute) (list absolute)))             ; ERROR
((lambda (a &optional &key absolute)
   (list a absolute))
 10)                                    ; (10 nil)

;; $ which chmod
(executable-find "chmod")               ; "c:/cygwin/bin/chmod.exe"

;; 結果がちょっと違う
(macroexpand '(do ((acc nil) (n 0 (1+ n))) ((> n 10) (nreverse acc))
               (push n acc)))
(cl-prettyexpand '(do ((acc nil) (n 0 (1+ n))) ((> n 10) (nreverse acc))
                   (push n acc)))

(cl-prettyexpand '(flet ((hoge () (list x (symbol-value 'x))))
                   (setq x 1)
                   (let ((x 10))
                     (hoge))))

(fset 'yes-or-no-p #'y-or-n-p)

(autoload 'mpg123 "mpg123" "A Front-end to mpg123/ogg123" t)

(defun buffer-list-by-name ()
  (sort (buffer-list)
        #'(lambda (x y)
            (string< (buffer-name x) (buffer-name y)))))

;; (buffer-list)の出力をソートする方法
(defun buffer-list-by-name ()
  (dolist (buffer (buffer-list-by-name))
    (bury-buffer buffer))
  (buffer-list))

(execute-kbd-macro "\M-\;")             ; 
(command-execute "\M-\;")               ; 

;; EOF以下のバッファいらない
;; うまく動いてない気がする
(defun fit-window ()
  (interactive)
  (when (pos-visible-in-window-p (point-max))
    (enlarge-window (- (buffer-lines)
                       (window-lines)
                       (get-window-start-line)))))
;; (add-hook 'buffer-menu-mode-hook 'fit-window)

max-lisp-eval-depth                     ; 300

;; EmacsWikiからインストール (byrubikich)
;; http://www.emacswiki.org/cgi-bin/emacs/install-elisp.el

;;; 一発インデント
(defun indent-buffer ()
  "indent current buffer"
  (interactive)
  (indent-region (point-min) (point-max)))

;; elispのデバッグ関数色々
;; デバッガ (debug, edebug)
;; http://www.bookshelf.jp/texi/elisp-manual/21-2-8/jp/elisp_18.html#Edebug
;; プロファイラ (profile)
;; http://www.mew.org/~kazu/doc/elisp/profile.html
(mapcar #'featurep '(profile elp disass debug edebug)) ; (nil nil nil t nil)

;; #1=(#1# x) でないの？
(let ((a '(x x))) (setcar a a))         ; (#0 x)
(let ((a '(x x))) (setcdr a a))         ; (x . #0)
(quote #1=(#1# x))                      ; (#0 x)

;; defun* (cl-macs.el) の使い道が分からん

;;; @@dependence
;;; elispの依存関係を調べる
(featurep 'loadhist)
(feature-file 'google)                  ; "c:/home/lxuser/lib/emacs/google.elc"
(feature-file 'cl)                      ; "c:/home/emacs/22.1/lisp/emacs-lisp/cl.elc"
(file-provides "cl")                    ; (cl cl-19)
(file-requires "cl")                    ; nil
(file-requires "xyzzy")                 ; (cl)
;; 'clがどのファイルからロードされているか
(file-dependents "cl")                  ; ("c:/home/emacs/22.1/lisp/emacs-lisp/cl-macs.elc" "c:/home/lxuser/lib/emacs/xyzzy-util.el" "c:/usr/local/clisp-2.47-full/lib/slime/hyperspec.elc" "c:/usr/local/clisp-2.47-full/lib/slime/slime.elc")

;; ?
(symbol-file 'cl)                       ; "c:/home/lxuser/lib/emacs/xyzzy.el"
(symbol-file 'xyzzy)                    ; "c:/home/lxuser/lib/emacs/xyzzy.el"
(symbol-file 'symbol-file)              ; "c:/home/emacs/22.1/lisp/subr.elc"

;; 一応残すけどいらないと思う
(defun sub-directory-p (dir parent)
  "DIRECTORYがPARENTのサブディレクトリならt、そうでなければnilを返す。"
  (labels ((dirname (x)
             "末尾に`/'をつけたディレクトリ名を返す: /home->/home/"
             (if (file-directory-p x)
                 (file-name-as-directory x)
                 (file-name-directory x))))
    ;; dirnameはfile-name-as-directoryだけでいいと思う
    (do ((x (pathname-directory (dirname dir)) (cdr x))
         (y (pathname-directory (dirname parent)) (cdr y)))
        ((null y) t)
      (if (or (null x)
              (not (equalp (car x) (car y))))
          (return nil)))))

;;; @@compile
;; 指定したディレクトリ以下を再バイトコンパイル
(byte-recompile-directory "~/lib/emacs/" t) ; *.elcのないファイルも強制的に？

;;; @@File-local Variables in Emacs
;;; http://www.kmc.gr.jp/~tak/memo/emacs-local-variable.html

(process-list)                          ; (#<process shell>)
(get-process "shell")                   ; #<process shell>

;; マウスホイール移動のときだけ avoidance-mode を無効にしたかったが、出来なかった
(defadvice mwheel-scroll (around no-mouse-avoidance-mode activate)
  (let ((mouse-avoidance-mode nil))
    ad-do-it))

;; tabbar-mode
;; http://www.emacswiki.org/cgi-bin/wiki/TabBarMode

;; 概観変更とグループ化の変更
;; http://d.hatena.ne.jp/katsu_w/20080319
;; http://amitp.blogspot.com/2007/04/emacs-buffer-tabs.html

;; 開発が再開された？有志によるものか？
;; http://github.com/davidswelt/aquamacs-git/tree/master/src/site-lisp/tabbar/tabbar.el
(load-file "~/lib/tabbar.el")
(tabbar-mode t)
(remove-hook 'kill-buffer-hook 'tabbar-buffer-track-killed)

(global-set-key "\C-x\C-t\C-f" 'toggle-truncate-lines)

;; elispのバッククオートの扱い (backquote.el)

;; なぜエラーになる？ -> elispに&bodyはない。&restを使うかdefmacro*を使うか
(defmacro when1 (test &body body)
  (let ((result (gensym)))
    `(let ((,result ,test))
       (when ,result ,@body)
       ,result)))
(when1 (position ?a "kobayashi")
       (princ "見つけた!"))

(setq )
(set-variable )

;;; 正規表現を書くのを支援するツール
(regexp-opt '("define" "lambda" "fn" "define-macro" "lambda-macro") t)
"\\(define\\(?:-macro\\)?\\|fn\\|lambda\\(?:-macro\\)?\\)"
(regexp-opt '("define" "lambda" "fn" "define-macro" "lambda-macro") 'words)
"\\<\\(define\\(?:-macro\\)?\\|fn\\|lambda\\(?:-macro\\)?\\)\\>"

;;; (0 1 2 3 4 ...) なんて略記いらない
(setq eval-expression-print-length nil
      eval-expression-print-level nil)
(list eval-expression-print-length eval-expression-print-level) ; (12 4)

;;; @@defadviceマクロ
(ad-is-active 'eval-last-sexp)
;; 活性化/不活性化
(ad-activate ad-deactivate)

;;; Emacs@vineのカーソル色付けってどうやってたっけ？
(list input-method-activate-hook input-method-inactivate-hook)
(mapcar #'boundp '(mw32-ime-on-hook mw32-ime-off-hook))
(lambda () (set-cursor-color "brown"))

;;; !? string-to-list@mule-util.el
(append "こばやし" nil)                 ; (53811 53840 53860 53815)

;;; @@etags
;; http://www8.atpages.jp/hotsuma/chalow/2002-04-16.html#2002-04-16-1
TAGS コマンドまとめ [Emacs]
M-x find-tag (M-.) シンボルの定義部分に飛ぶ。
M-x pop-tag-mark (M-*) 前の状態に飛ぶ。
M-x tags-search シンボルを検索させる。
M-x tags-loop-continue (M-,) 次の候補に飛ぶ。(検索状態で実行)
M-x find-tag-other-window (C-x 4 .) 定義部分を別ウィンドウに表示。
M-x find-tag-other-frame (C-x 5 .) 定義部分を別フレームに表示。
M-x complete-symbol (M-TAB) シンボル名の補完(タブテーブルが読み込まれている時)
M-x visit-tags-table タブテーブルを読み込み直す。

(defun what-charset-region (from to)
  (interactive "r")
  (message "%s" (find-charset-region from to)))

frame-title-format
header-line-format
mode-line-format
icon-title-format

;; 相対ディレクトリ？
(expand-file-name (file-relative-name "c:/home/TODO.txt")) ; "c:/home/TODO.txt"

(list most-positive-fixnum most-negative-fixnum) ; (268435455 -268435456)

(defun redraw-emacs ()
  (redraw-display)
  (redraw-frame (selected-frame))
  (redraw-modeline t))

(every #'char-valid-p (number-sequence #x00 #xFF)) ; t

;; 関数、マクロ、変数を未定義化する
;; 依存関係が解消されないとエラー (他のファイルから require されてるとか)
(require 'redo)
(unload-feature 'redo)

;;; コマンドラインから
command-line-args                       ; ("C:\\home\\emacs\\22.1\\bin\\emacs.exe")
;;; スクリプト言語として利用
noninteractive                          ; nil (バッチ処理時 t)
$ emacs --script (Emacs 22 以降のオプション)
$ emacs --batch -Q -l 2> /dev/null
;; --quick, -Q (equivalent to -q --no-site-file --no-splash)
;;; バッチファイルからコンパイル
;;; 後で調べろ
$ emacs -batch -q -no-site-file -l $FILE

$ emacs -batch -f batch-byte-compile *.el

;; #! がコメント行として無視される (これは興味深い)
(apply #'+
       #! (error "comment?")
       '(1 2 3 4 5 6 7 8 9 10))         ; 55

;;; auto-complete-mode
(require 'auto-complete)
(global-auto-complete-mode t)

(defun run-newlisp ()
  (interactive)
  ;; run-scheme?
  (let ((default-process-coding-system '(utf-8 . utf-8)))
    (run-scheme (format "C:/PROGRA~1/newlisp/newlisp.exe -C -w %s"
                        (expand-file-name default-directory)))))

;;; 全角空白、タブなどの強調表示
;;; これだとfont-lockが呼び出されるたびにキーワードが追加されて遅くならないか？
;;; http://eigyr.dip.jp/diary/200712.html#%E5%85%A8%E8%A7%92%E3%82%B9%E3%83%9A%E3%83%BC%E3%82%B9%E3%82%84%E3%82%BF%E3%83%96%E3%82%92%E5%BC%B7%E8%AA%BF

;;; @@Info
(defun info-elisp-manual ()
  "Display the Emacs Lisp manual in Info mode."
  (interactive)
  (info "elisp"))
(Info-goto-node)                        ; info-mode でインタラクティブに呼び出してみる(g)
(call-interactively #'info-lookup-symbol)
(directory-files (car Info-directory-list) nil "elisp*")
(info "(elisp)Regular Expressions")
(info "(elisp)Mapping Functions")
(info "(elisp)Definition of signal")
(info "(elisp)Definition of mapatoms")
(info "(libc)Formatted Output Functions")

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

;; 補完等
;; http://www.bookshelf.jp/texi/elisp-manual/21-2-8/jp/elisp_20.html#SEC270
completion-ignore-case                                        ; t
(try-completion "f" '("foo" "bar" "baz" "bazz" "hoge" "for")) ; "fo"
(try-completion "b" '("foo" "bar" "baz" "bazz" "hoge" "for")) ; "ba"
(all-completions "f" '("foo" "bar" "baz" "bazz" "hoge" "for")) ; ("foo" "for")

(defun list-hexadecimal-colors-display ()
  (interactive)
  (list-colors-display
   (mapcar #'(lambda (x)
               (format "#%.6x" x))
           (number-sequence #x000000 #xe0e0e0 #x000020))))
(byte-compile 'list-hexadecimal-colors-display)
(logand #b0111)
(format "%x" #b0111)
(logand #b01111 #b0001)
(defun logcount (integer)
  (do ((n 0 (1+ n))
       (i integer (lsh)))))

;;; @@Bitwise Operations on Integers
;; lsh (logical shift: 論理シフト)
(lsh #b0101 #b0001)                     ; 10 #b1010
(lsh #b0111 #b0001)                     ; 14 #b1110
(lsh #b0110 -1)                         ;  3 #b0011

;; ash (arithmetic shift: 算術シフト) 
(ash #b11111111111111111111111111010    ; -6
     #b11111111111111111111111111111    ; -1
     )                                  ; -3 #b11111111111111111111111111101

(encode-coding-string "こばやし" 'cp932)  ; "\202\261\202\316\202\342\202\265"
(encode-coding-string "こばやし" 'binary) ; "\222\244\263\222\244\320\222\244\344\222\244\267"
(encode-coding-string "こばやし" 'sjis)   ; "\202\261\202\316\202\342\202\265"
(encode-coding-string "こばやし" 'euc-jp) ; "\244\263\244\320\244\344\244\267"
(encode-coding-string "こばやし" 'utf-8)  ; "\343\201\223\343\201\260\343\202\204\343\201\227"
(encode-coding-string "こばやし" 'utf-16) ; "\376\3770S0p0\2040W"

(string-as-multibyte (string-as-unibyte "こばやし")) ; "こばやし"

(decode-sjis-char (encode-sjis-char ?z)) ; 122 (#o172, #x7a, ?z)

(normal-top-level-add-to-load-path DIRS)

;; ファイルを連番にする
;; ただし、既にソートされている必要あり
(let ((n 0)
      (ext "jpg"))
  (dolist (file (directory-files #1=DIR nil (format "\\.%s$" ext)))
    (rename-file (expand-file-name file #1#)
                 (expand-file-name (format "%04d.%s" n ext) #1#))
    (setq n (1+ n))))

(defun list-to-string (char-list) (apply #'string char-list))
(list-to-string (string-to-list "これは漢字文字列")) ; "これは漢字文字列"

;; Y combinator ぽいもの
(defun f (q)
  (lexical-let ((q q))
    (lambda (n)
      (if (= n 0) 1 (* n (funcall (funcall q q) (- n 1)))))))
(funcall (f 'f) 10)                     ; 3628800

;;; help-fns.el:describe-function-1:253
(defun function-truename (def)
  "DEFが関数のエイリアスならば、関数の実体名を返す."
  (while (symbolp (symbol-function def))
    (setq def (symbol-function def)))
  def)

(local-variable-if-set-p 'truncate-lines) ; t
(local-variable-p 'truncate-lines)        ; nil

;; http://condotti.blogspot.com/2007_06_01_archive.html
(defsubst buffer-bytes (buffer)
  "Return number of bytes in a buffer."
  (with-current-buffer buffer
    (1- (position-bytes (point-max)))))
;; これ、ファイルサイズとは違うのかね？
(list 
 (buffer-bytes ".emacs.my.el")
 (buffer-size (get-buffer ".emacs.my.el"))
 (nth 7 (file-attributes "~/.emacs.my.el"))
 )                                      ; (16404 16854 14852)

(format-time-string "%Y-%m-%dT%T%z")    ; "2009-04-14T04:44:54+0900"
;; UNIX time
(decode-time '(0 0 0))                  ; (0 0 9 1 1 1970 4 nil 32400)
;; Universal Time
(multiple-value-list
 (decode-universal-time 0))             ; (0 0 9 1 1 1900 0 NIL -9) [CL]

(defun symbol-describe (symbol)
  `((:name     ,(symbol-name symbol))
    (:value    ,(and (boundp symbol)
                     (symbol-value symbol)))
    (:function ,(and (fboundp symbol)
                     (if (byte-code-function-p #1=(symbol-function symbol))
                         symbol #1#)))
    (:plist    ,(symbol-plist symbol))
    (:file     ,(symbol-file symbol))))

(defun* bindp (key &optional (map global-map))
  (lookup-key map key))
