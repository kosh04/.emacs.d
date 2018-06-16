;;; memo/emacs-lisp.el          -*- mode: emacs-lisp; lexical-binding: t; -*-

;;; ELisp/CL の違い
;; 102: Emacs Lisp と Common Lisp は似ているのですか?
;; http://stuff.mit.edu/afs/athena/astaff/project/babel/build/sun4/etc/FAQ.jp
;; - 大文字小文字を区別する
;; - 動的スコープ
;; - パッケージが無い
;; - 多値が無い
;; - リーダマクロが無い
;; - 有理数、不動小数、多倍長整数が無い

;; クオートでコメントアウトしたS式はコメントの色にする
;; http://d.hatena.ne.jp/rubikitch/20080413/1208029110
(defun elisp-font-lock-top-quote (limit)
  (when (re-search-forward "^' *(" limit t)
    (forward-char -1)
    (set-match-data (list (point) (progn (forward-sexp 1) (point))))
    t))

(font-lock-add-keywords 'emacs-lisp-mode
  '((elisp-font-lock-top-quote 0 font-lock-comment-face prepend)))

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
;; 遅延評価で対処
;; 1. add-hook
(add-hook 'hoge-mode-hook
          (lambda ()
            (define-key hoge-map [up] 'hoge-hoge)))
;; 2. eval-after-load
(eval-after-load "hoge"
  (quote
   (progn
     (define-key hoge-map [up] 'hoge-hoge))))

;; :key 使いづらくないか？
(funcall (lambda (&key absolute) (list absolute)) :absolute t) ; (t)
(funcall (lambda (&key absolute) (list absolute)))             ; ERROR
(funcall (lambda (a &optional &key absolute)
           (list a absolute))
         10)                            ; (10 nil)

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

;; @@Debug
;; elispのデバッグ関数色々
;; デバッガ (debug, edebug)
;; http://www.bookshelf.jp/texi/elisp-manual/21-2-8/jp/elisp_18.html#Edebug
(require 'disass)
(require 'debug)
;;; pp.el --- pretty printer for Emacs Lisp
(require 'pp)

;; プロファイラ (profile) -> (OBSOLETE; use elp.el instead)
;; http://www.mew.org/~kazu/doc/elisp/profile.html
(require 'elp)
(require 'benchmark)
(require 'profiler)

;; Emacs Native Profiler
;; http://cx4a.org/pub/tokyo-emacs/emacs-native-profiler.pdf
(progn
  (profiler-start 'cpu)
  (list-packages)
  (profiler-report))

;; a source-level debugger for Emacs Lisp
;; C-u C-M-x で関数をデバッグ定義. 関数を再定義すればデバッグは解除される.
(require 'edebug)

;; trace.el ---  CL の trace/untrace に近いトレース評価
;; http://clhs.lisp.se/Body/m_tracec.htm
(defun fact (n) (if (zerop n) 1 (* n (fact (- n 1)))))
(trace-function 'fact)
(fact 3)
(untrace-function 'fact)

(defmacro d (expr)
 `(let ((_var (eval ',expr)))
    (run-at-time 0 nil 'display-buffer "*Messages*")
    (message "%S=%S" ',expr _var)
    _var))

;; キー入力を保存する ([(help) l] みたいなの)
(open-dribble-file "~/.dribble.el")

;; #1=(#1# x) でないの？
(let ((a '(x x))) (setcar a a))         ; (#0 x)
(let ((a '(x x))) (setcdr a a))         ; (x . #0)
(quote #1=(#1# x))                      ; (#0 x)

;;; @@dependence
;;; 依存関係を調べる
(require 'loadhist)
(feature-file 'google)                  ; "c:/home/lxuser/lib/emacs/google.elc"
(feature-file 'cl)                      ; "c:/home/emacs/22.1/lisp/emacs-lisp/cl.elc"
(file-provides "cl")                    ; (cl cl-19)
(file-requires "cl")                    ; nil
(file-requires "xyzzy")                 ; (cl)
;; 'clがどのファイルからロードされているか
(file-dependents (feature-file 'cl))
;; ("/usr/share/emacs/22.2/lisp/emacs-lisp/cl-macs.elc"
;;  "/usr/share/emacs22/site-lisp/slime/hyperspec.elc"
;;  "/usr/share/emacs22/site-lisp/slime/slime.elc"
;;  "/usr/share/doc/git-core/contrib/emacs/vc-git.el")

;; ?
(symbol-file 'cl)                       ; "c:/home/lxuser/lib/emacs/xyzzy.el"
(symbol-file 'xyzzy)                    ; "c:/home/lxuser/lib/emacs/xyzzy.el"
(symbol-file 'symbol-file)              ; "c:/home/emacs/22.1/lisp/subr.elc"

(defun symbol-source-file (symbol)
  "[user] SYMBOL が定義されているファイル名を返します."
  (let ((type (cond ((fboundp symbol) (symbol-function symbol))
                    ((boundp symbol) 'defvar)
                    (t nil))))
    (find-lisp-object-file-name symbol type)))

;; (symbol-source-file 'car)       ;=> C-source (symbol)
;; (symbol-source-file 'dired)     ;=> "/path/to/lisp/dired.el"
;; (symbol-source-file 'foo)       ;=> nil

(add-to-list 'minor-mode-alist '(debug-on-error " (ﾟдﾟ)"))
(toggle-debug-on-error)

(abort-recursive-edit)
(exit-recursive-edit)
(quit-recursive-edit)                   ; alias for `abort-recursive-edit'

;;; byte-compile / byte-code
;;; ------------------------
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Byte-Compilation.html

;; 指定したディレクトリ以下を再バイトコンパイル
(byte-recompile-directory "~/lib/emacs/" t) ; *.elcのないファイルも強制的に？

(defun fact (n) (if (zerop n) 1 (* n (fact (- n 1)))))
(byte-compile 'fact)
;; バイトコードは実行可能
(#[(n) "\301\010!\203\010\000\302\207\010\303\010S!_\207" [n zerop 1 fact] 3]
 10)
;;=> 3628800

;; 逆アセンブラ
(require 'disass)

(disassemble #'fact)
;; byte code for fact:
;;   args: (n)
;; 0	constant  zerop
;; 1	varref	  n
;; 2	call	  1
;; 3	goto-if-nil 1
;; 6	constant  1
;; 7	return	  
;; 8:1	varref	  n
;; 9	constant  fact
;; 10	varref	  n
;; 11	sub1	  
;; 12	call	  1
;; 13	mult	  
;; 14	return	  

;; Elisp Bytecode アセンブラ (non-official)
(require 'easm)
(equal (easm '(x)
	     '((varref x)
	       (add1)
	       (varref x)
	       (sub1)
	       (cons)
	       (return)))
       (byte-compile
	#'(lambda (x)
	    (cons (+ x 1) (- x 1)))))
;;=> t


;; C-u C-M-x (eval-defun) で edebug 起動

;; Eldoc は便利です
;; ** Emacs23のeldocは対応する仮引数がハイライトされるようになっている。
;; http://d.hatena.ne.jp/rubikitch/20090207/1233936430
;; (load-file "c:/home/emacs/22.1/lisp/emacs-lisp/eldoc.el")
(autoload 'turn-on-eldoc-mode "eldoc" nil t)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)

;; xyzzy.el でなんとかなる
(defun my:macroexpand-region (from to &optional full)
  (interactive "r\nP")
  (load "cl-extra")                     ; cl-prettyexpand
  (let* ((form (read-from-string (buffer-substring from to))))
    (with-output-to-temp-buffer #1="*elisp macroexpand*"
      (with-temp-buffer
        (cl-prettyexpand (car form) full)
        (copy-to-buffer #1# (point-min) (point-max))))))
(defalias 'macroexpand-region 'my:macroexpand-region)

;; ラムダ式の比較
(equal #'(lambda (x) (+ x x))
       #'(lambda (x) (+ x x)))          ; t

;; レキシカルな束縛
(lexical-let ((count 0))
  (defun counter ()
    (incf count)))
(list (counter) (counter))              ; (1 2)

max-lisp-eval-depth                     ; 300 (default)

max-specpdl-size
;; http://www.bookshelf.jp/texi/elisp-manual/21-2-8/jp/elisp_11.html#IDX595
;; Limit on number of Lisp variable bindings and `unwind-protect's.
;; = 変数束縛と unwind-protect による後始末の個数の制限

;; この辺はエラーになったからといって不用意に値を増やすべきではないんじゃなかろうか
;; 先にエラーの原因となりえるコードを疑うべき

;; EmacsWikiからインストール (byrubikich)
;; http://www.emacswiki.org/cgi-bin/emacs/install-elisp.el

;; elispのバッククオートの扱い (backquote.el)
(symbol-function #'\`)
;;=> (macro . #[(arg) "\301!A\207" [arg backquote-process] 2 887462])
`(1 2 3 ,(+ 1 2 3))                     ; (1 2 3 6)
(macroexpand '`(1 2 3 ,(+ 1 2 3)))      ; (list 1 2 3 (+ 1 2 3))

;; なぜエラーになる？ -> elispに&bodyはない。&rest か cl-defmacro を使う
(defmacro when1 (test &body body)
  (let ((result (gensym)))
    `(let ((,result ,test))
       (when ,result ,@body)
       ,result)))
(when1 (position ?a "emacs lisp")
  (princ "見つけた!"))

(setq )
(set-variable )

;; (0 1 2 3 4 ...) なんて略記いらない
(setq eval-expression-print-length nil
      eval-expression-print-level nil
      eval-expression-debug-on-error nil)

(list eval-expression-print-length
      eval-expression-print-level
      eval-expression-debug-on-error)
;;=> (12 4 t)

(setq print-circle nil)

;; (1 2 ...) <-> (1 2 3 4 5) の切り替え時にメッセージ
;; そもそも"..."の表示自体あまりいらないんだけど
(defadvice last-sexp-toggle-display (after with-message activate)
  (if (save-excursion
        (backward-char 3)
        (looking-at (regexp-quote "...)")))
      (message "Fold last sexp")
      (message "Expand last sexp")))

;;; mule-util.el
(defsubst string-to-list (string)
  "Return a list of characters in STRING."
  ;; `append' の末尾が nil ならば文字型のリストに変換する (cl-coerce@cl-extra.el も参照)
  (append string nil))
(append "こんにちは" nil)                 ; (53811 53840 53860 53815)

(cl-loop for sym
         in '(most-negative-fixnum
              most-negative-short-float
              most-negative-double-float
              most-negative-single-float
              most-positive-single-float
              most-positive-long-float
              most-positive-short-float
              most-positive-fixnum
              most-negative-long-float
              most-positive-double-float)
         if (boundp sym)
         collect (cons sym (symbol-value sym)))
;;=> ((most-negative-fixnum . -268435456) (most-positive-fixnum . 268435455))
;;=> ((most-negative-fixnum . -2305843009213693952) (most-positive-fixnum . 2305843009213693951))

;;; 2つの関数の違いは?
(interactive-p)
;; Return t if the function was run directly by user input.
;; This means that the function was called with `call-interactively'
;; (which includes being called as the binding of a key)
;; and input is currently coming from the keyboard (not in keyboard macro),
;; and Emacs is not running in batch mode (`noninteractive' is nil).
;;
;; The only known proper use of `interactive-p' is in deciding whether to
;; display a helpful message, or how to display it.  If you're thinking
;; of using it for any other purpose, it is quite likely that you're
;; making a mistake.  Think: what do you want to do when the command is
;; called from a keyboard macro?
;;
;; If you want to test whether your function was called with
;; `call-interactively', the way to do that is by adding an extra
;; optional argument, and making the `interactive' spec specify non-nil
;; unconditionally for that argument.  (`p' is a good way to do this.)

(called-interactively-p)
;; Return t if the function using this was called with `call-interactively'.
;; This is used for implementing advice and other function-modifying
;; features of Emacs.
;;
;; The cleanest way to test whether your function was called with
;; `call-interactively' is by adding an extra optional argument,
;; and making the `interactive' spec specify non-nil unconditionally
;; for that argument.  (`p' is a good way to do this.)

;; ## 出力関数 (print, prin1, princ)

;; 出力する文字毎に関数を呼び出す
(print :こんにちは (lambda (c) (message "recv: %c" c)))
;; recv: \n
;; recv: :
;; recv: こ
;; recv: ん
;; recv: に
;; recv: ち
;; recv: は
;; recv: \n
;; -> :こんにちは

;; ## Emacsに関する環境変数
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/General-Variables.html

;; $EMACSPATH
;; 設定されていれば exec-path にパスとして追加される
(length exec-path)                          ;=> 57
(length (parse-colon-path (getenv "PATH"))) ;=> 48

;; Docstring の書式
;; ================

;; - http://www.gnu.org/software/emacs/manual/html_node/elisp/Documentation-Tips.html
;; - https://www.emacswiki.org/emacs/DocString
;; - http://ergoemacs.org/emacs/inline_doc.html

(defun docstring-example-mode (&optional command)
  "`forward-char' is bind to \\[forward-char].

The following commands are available:

\\{help-mode-map}"
  (interactive)
  (message "%s" (documentation 'docstring-example)))
;; `SYMBOL'   -> シンボルのdocstringへのリンク
;; \[COMMAND] -> コマンドに割り当てられたキー
;; \{KEYMAP}  -> キーマップ一覧
;; \<KEYMAP>  -> ??
;; ARGS -> 関数の引数は大文字 : (lambda (column arg) "Set COLUMN with ARG." ...)
;; URL `http://~'  -> Clickable URL
;; `(emacs) Dired' -> Info へのリンク
;; 先頭に `*' があるのはどういう場合？
;;   `set-variable' 時に区別するため(?)
;;   - https://www.gnu.org/software/emacs/manual/html_node/eintr/defvar-and-asterisk.html
;;   - https://www.emacswiki.org/emacs/VariableDefinition
;; M-x checkdoc を参考

(substitute-command-keys "\\[foward-char]") ;;=> "M-x foward-char"
(substitute-command-keys "\\{ctl-x-map}")

;; 排他制御
(setq create-lockfiles nil)

;; syntax / 構文解析
;; - https://www.gnu.org/software/emacs/manual/html_node/elisp/Position-Parse.html
;; - https://www.gnu.org/software/emacs/manual/html_node/elisp/Parser-State.html

(cl-defstruct (parser-state
               (:constructor parser-state
                (&aux
                 (_state (syntax-ppss))
                 (depth (nth 0 _state))
                 (bos (nth 1 _state))
                 (eos (nth 2 _state))
                 ;;...
                 )))
  "State of the syntactic parser."
  depth                                 ; 括弧の深さ
  bos ; 最も内側のリストの開始位置 (position of the start of the innermost parenthetical grouping containing the stopping point)
  eos ; 最後のS式の開始位置 (position of the start of the last complete subexpression terminated)
  stringp                               ; 文字列内部にいるかどうか
  non-nextable-comment-p                ; コメントの内部にいるかどうか?
  end-point-is-just-after-a-quote-character
  minimum-parenhesis-depth
  comment-style
  string-or-comment-start-position
  internal-data
  )

;; Base64 符号化ついでにエンコードorデコードも行う
(defun base64-encode-region* (start end coding-system)
  (interactive "*r\nzCoding system: ")
  (save-restriction
    (narrow-to-region start end)
    (encode-coding-region start end coding-system)
    (base64-encode-region (point-min) (point-max))))

;; 型と構文
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Type-Predicates.html
;; http://www.geocities.co.jp/SiliconValley-Bay/9285/ELISP-JA/elisp_57.html

- atom nil t
- array []
- bool-vector (make-bool-vector 5 t) #&5""
- buffer #<buffer *scratch*>
- compiled-function  #[257 "..." [...] 9 "docstring"]
- case-table (standard-case-table) #^[...]
- char-or-string
- char-table (make-char-table 'foo) #^[nil ...]
- command
- cons (X . Y)
- custom-variable
- display-table (make-display-table) #^[nil ...]
- float pi => 3.141592653589793
- font
- frame-configuragion
- frame-live
- frame
- function #'(lambda () ...) #'car
- hash-table (make-hash-table) #s(hash-table ...)
- integer-or-marker
- integer
- keymap (keymap #^[...]) global-map
- keyword :keyword
- list (a b c ...)
- marker
- wholenum
- nilst (nlistp OBJ) == (not (nlistp OBJ))
- number
- number-or-marker
- overlay (make-overlay 0 0) #<overlay from 1 to 1 in emacs-lisp.el>
- process #<process ielm>
- sequence (LIST or VECTOR or STRING)
- string "Hello, World!"
- subr #'car
- symbol
- syntax-table
- vector
- window-configuration
- window-live
- window
- boolean nil t
- string-or-null
- font-spec (font-spec ...) #<font-spec ...>

;; tetris.el のソースを読むと参考になる
;; http://d.hatena.ne.jp/goinger/20070713/1184316777
(describe-function #'tetris)

;;; Customization

(info "(elisp) Customization")

;; かんたん assoc list
(defmacro @ (&rest kvs)
  `(cl-loop for (#1=#:key . #2=#:value) in ',kvs
            collect (cons #1# (eval #2#))))

(@ (num . (+ 1 2 3))
   (key . #'number-to-string)
   (#1=lisp . (format "%s" '#1#)))
;;=> ((num . 6) (key . number-to-string) (lisp . "lisp"))

(defun describe-url-generic (url)
  (let ((x (url-generic-parse-url url)))
    (list :type (url-type x)
          :user (url-user x)
          :password (url-password x)
          :host (url-host x)
          :portspec (url-portspec x)
          :port (url-port x)
          :filename (url-filename x)
          :path-and-query (url-path-and-query x)
          :target (url-target x)
          :attributes (url-attributes x)
          :fullness (url-fullness x))))

;; 静的束縛と動的束縛の違い
;; https://www.reddit.com/r/emacs/comments/4f7q0f
(setq lexical-binding t)
(let (xx)
  (setq xx :setq)
  (set 'xx :set) ;; set global
  xx)
;;=> :set  (eq lexical-binding nil)
;;=> :setq (eq lexical-binding t)

;; バイナリデータの読み込み
(require 'bindat)

(bindat-unpack '((:signature str 8)
                 (:ihdr str 25))
               (f-read-bytes "~/Pictures/bg.png"))

(defun process-describe (proc)
  (list ;;:attributes (process-attributes proc)
        :buffer (process-buffer proc)
        :coding-system (process-coding-system proc)
        :command (process-command proc)
        :contact (process-contact proc)
        :datagram-address (process-datagram-address proc)
        :exit-status (process-exit-status proc)
        :filter (process-filter proc)
        :sentinel (process-send-eof proc)
        :status (process-status proc)
        :type (process-type proc)
        :plist (process-plist proc)
        ))

;; 行頭のプロパティを取得
(hack-local-variables-prop-line)
;;=> ((lexical-binding . t) (mode . emacs-lisp))

;; パッケージのメタデータを取得 (lisp-mnt.el)

(lm-get-package-name) ;;=> "memo/emacs-lisp.el"
(lm-summary)
(lm-authors)
(lm-maintainer)
(lm-creation-date)
(lm-last-modified-date)
(lm-version)
(lm-keywords)
(lm-keywords-list)
(lm-keywords-finder-p)
(lm-adapted-by)
(lm-commentary)
(lm-homepage)

(save-excursion
  (lm-header "version"))

(lm-verify file showok verbose non-fsf-ok)

(locate-library "lisp-mnt")

;; ファイルあるいはディレクトリのelispパッケージの概要を表示する
M-x lm-synopsis

;; バグレポート
M-x report-emacs-bug ;; GNU Emacs に関するバグレポート
M-x lm-report-bug    ;; パッケージ作者宛にバグレポート

;; パッケージの依存関係をツリー表示
;; https://www.emacswiki.org/emacs/lib-requires.el

;; lisp-mnt 拡張
;; https://github.com/tarsius/elx

;; 行番号
(require 'linum)
;; 行番号の表示にもう少しゆとりを
(setq linum-format (lambda (line)
                     (propertize (format (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
                                           (concat "%" (number-to-string w) "d "))
                                         line)
                                 'face 'linum)))

;; obsolete
(define-obsolete-function-alias 'foo-lold 'foo "25.1")
(defun foo ()
  (declare (obsolete foo-old "25.1"))
  nil)

;; クリップボードのデータをkillリングに保存する
(setq save-interprogram-paste-before-kill t)

;; http://stackoverflow.com/q/3072648
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; emacs-lisp におけるバージョン番号の割り当て方法は `version-to-list' を参照

;; ## バグっぽい ##
;; `eval-when-compile' を利用してバイトコンパイルを行う前に
;; そのファイルが読み込み済みであった場合、内部の変数の値が古い可能性がある
(defvar foo-x 10)
(defvar foo-y (eval-when-compile (+ foo-x 20)))
;; 上記のコードを読み込んだ後、(defvar foo-x 99) に変更してバイトコンパイルすると
;; `foo-x' の値は defvar の性質上「更新されない」ため (defvar foo-y 30) となってしまう
;;
;; ## 起こりうる問題 ##
;; パッケージの更新時。ファイルのダウンロード後にバイトコンパイルも行うため
;; 上記の問題が起こるかもしれないため注意が必要
;;
;; ## 対策 ##
;; (本来であればバイトコンパイルは状態を持たない素の Emacs で行うべき)
;; * .elc ファイルを削除して Emacs を再起動後に改めてバイトコンパイルする
;; * バイトコンパイルを外部プロセスを呼び出して行う (`emacs -Q -batch ...`)

;; Group, Customization etc
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Customization.html

;; ";;; Commentary:" 部分を抽出する (ヘルプ表示用)
(finder-commentary (locate-library "bs"))

;; コマンドを実行して文字列化
(shell-command-to-string "gcc -v") ;;=> "..."
(process-lines "gcc" "-v")         ;;=> ("..."  ...)


(with-eval-after-load 'nameless
  ;; which-function-mode でも強制的に nameless 表示にする
  ;; 自作した (Elisp の命名規則に則っていない) 関数の表示がされなくなるバグあり
  (require 'which-func)
  (require 's)
  (defun user/nameless-compose-name (prefix name)
    (let ((name* (substring name (length prefix))))
      (cond ((and (s-starts-with-p "--" name*) nameless-private-prefix)
             (concat nameless-prefix nameless-prefix (substring name* 2)))
            ((s-starts-with-p "-" name*)
             (concat nameless-prefix (substring name* 1)))
            (t name*))))
  (defconst which-func-current
    '(:eval (replace-regexp-in-string
             "%" "%%"
             (let ((prefix nameless-current-name)
                   (name (gethash (selected-window) which-func-table)))
               (if (and nameless-mode (s-starts-with-p prefix name))
                   (user/nameless-compose-name prefix name)
                 (or name which-func-unknown))))))
  )

;; ハッシュ構文はそのまま利用していいのか？
(let ((h #s(hash-table data(:k1 "v1" :k2 "v2"))))
  (gethash :k2 h))                      ;=> "v2"

(interactive-form 'next-line) ;;=> (interactive "^p\np")

(defun lipsum ()
  "Create a Lorem Ipsum dummy text."
  (with-temp-buffer
    (url-insert-file-contents "http://loripsum.net/api")
    (shr-render-region (point-min) (point-max))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun disable-theme* ()
  (mapc #'disable-theme custom-enabled-themes))

;; Add ~/.emacs.d/vendor/*/ to load-path
(let ((vendor-dir (locate-user-emacs-file "vendor")))
  (dolist (name (directory-files vendor-dir))
    (let ((path (expand-file-name name vendor-dir)))
      (when (and (file-directory-p path)
                 (not (member name '("." ".."))))
        (message "add %s" path)
        (add-to-list 'load-path path)))))

;; パッケージに使えるキーバインドは C-c C-<key>
;; ユーザ用に使うためのキーバインド C-c <key>
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Conventions.html

focus-in-hook
focus-out-hook

;; コマンドライン文字列を分解
(split-string-and-unquote "gcc -Wall \"hello.c\"")
;;=> ("gcc" "-Wall" "hello.c")

;; gitのリモートURLを取得/ブラウジングする
;; https://github.com/rmuslimov/browse-at-remote
(let ((url (with-temp-buffer
             (vc-git--call t "ls-remote" "--get-url")
             (s-trim (buffer-string)))))
  (browse-url url))
