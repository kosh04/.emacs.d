;;; memo/emacs-lisp.el          -*- mode: lisp-interaction; lexical-binding: t; -*-

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

;; https://vmi.jp/software/emacs/edebug.html

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

(bindat-unpack
 '((:signature str 8)
   (:ihdr str 25))
 (f-read-bytes
  (expand-file-name "images/icons/hicolor/32x32/apps/emacs.png" data-directory)))
;;=> ((:ihdr . "\0\0\0\015IHDR\0\0\0 \0\0\0 \010\006\0\0\0szz\364") (:signature . "\211PNG\015\n\032\n"))

(defun process-describe (proc)
  (list ;;:attributes (process-attributes proc)
        :buffer (process-buffer proc)
        :coding-system (process-coding-system proc)
        :command (process-command proc)
        :contact (process-contact proc)
        :datagram-address (if (fboundp 'process-datagram-address)
                              (process-datagram-address proc)
                            '#:undef)
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

(defun user/frame-config (frame)
  "現在のフレームサイズと同じフレームを作成したい. (not worked yet)"
  (message "prev:%S,frame:%S" (previous-frame) frame)
  (pcase (frame-parameter (previous-frame) 'fullscreen)
    ('maximized (toggle-frame-maximized))
    ('fullboth  (toggle-frame-fullscreen))))

(add-hook 'after-make-frame-functions 'user/frame-config)

;; gitのリモートURLを取得/ブラウジングする
;; https://github.com/rmuslimov/browse-at-remote
(let ((url (with-temp-buffer
             (vc-git--call t "ls-remote" "--get-url")
             (s-trim (buffer-string)))))
  (browse-url url))

;; 変数バインディングいろいろ
;; https://emacs-china.org/t/topic/5396

;; Key-Value なデータをバインディング (assoc-list/property-list/hash-tablee)
(map-let (one zero) '((one . 1) (two . 2) (three . 3))
  (list one zero))
;;=> (1 nil)
(map-let (one zero) #s(hash-table data (one 1 two 2 three 3))
  (list one zero))
;; => (1 nil)
(pcase-let (((map one zero) '((one . 1) (two . 2) (three . 3))))
  (list one zero))
;;=> (1 nil)
(let-alist '((one . 1) (two . 2) (three . 3))
  (list .one .zero))
;;=> (1 nil)
(-let [(&plist 'one one 'zero zero) '(one 1 two 2 three 3)]
  (list one zero))
;;=> (1 nil)
(cl-loop for (x . y) in '((one . 1) (two . 2) (three . 3)) do ...)

;; Emacs 27.1 にて map.el が plists に対応する予定 (from NEWS)

;; nntpプロトコルを開きたい
(url-generic-parse-url "nntp://news.gmane.org/gmane.emacs.gnus.general")
;;=> #s(url "nntp" nil nil "news.gmane.org" nil "/gmane.emacs.gnus.general" nil nil t nil t t)
(defun url-nntp (url)
  (let ((gnus-select-method
         (list 'nntp (url-host url))))
    (gnus)))

;; Surpress byte-compile warning (Patch for `erefactor')
(unless (fboundp 'elisp--preceding-sexp)
  (defalias 'elisp--preceding-sexp #'preceding-sexp))

;;(setq ffap-machine-p-known 'reject)

;; a.txt の内容を正規表現で書き換えて b.txt に保存する
((lambda (infile outfile)
   (with-temp-buffer
     (insert-file-contents infile)
     (while (re-search-forward (rx "REGEXP") nil t)
       (replace-match "NEWTEXT"))
     (write-region (point-min) (point-max) outfile)))
 "a.txt" "b.txt")

;; コントロール文字等をできる限りエスケープして出力する
(let ((print-escape-multibyte t)
      (print-escape-nonascii t)
      (print-escape-control-characters t)
      (print-escape-newlines t))
  (prin1 "\xfe\xff\n\0\a\b\c\d\e\f\g🍣\n"))
;;-> "\376\377\n\0\007\010c\177\033\fg\x1f363\n"

(let ((print-circle t)
      (print-charset-text-property t)
      (print-continuous-numbering t)
      ;;(print-number-table )
      (print-gensym t)
      (print-length nil)
      (print-level nil)
      (print-quoted t)
      (print-escape-control-characters t)
      (print-escape-multibyte t)
      (print-escape-nonascii t)
      (print-escape-control-characters t)
      (print-escape-newlines t))
  (with-output-to-temp-buffer "*tmp*"
    (pp "OBJECT-HERE")))

;; 改行コード(U+23CE;⏎)を表示する
(setq buffer-display-table (make-display-table))
(aset buffer-display-table ?\^J
      (vector (make-glyph-code ?\u23ce 'escape-glyph)
              (make-glyph-code ?\^J 'escape-glyph)))

(set-display-table-slot standard-display-table 'truncation ?…)
(set-display-table-slot standard-display-table 'wrap       ?↩︎)

;; [2018-08-XX] <kbd>D</kbd> キーが押下できなくなったため回避方法いろいろ
;; - C-x 8 RET 0064 (?\u0064 == ?d)
;; - ソフトウェアキーボード
(global-set-key [kp-delete]  "d")        ; fn+delete
(global-set-key [deletechar] "d")        ; for -nw

;; バイトコンパイル時の警告 "Unused variable" を抑制する方法
;; - `ignore' マクロを利用する (ignore varname)
;; - 変数名の先頭にアンダーラインを付与する `_varname'

;; shell-command.elがあればミニバッファからのコマンド補完が便利になる
;; http://namazu.org/~tsuchiya/elisp/shell-command.el
;; (require 'shell-command)
;; (featurep 'shell-command)
(with-eval-after-load "shell-command"
  (defun filter-region (command &optional start end)
    (interactive (progn
                   (barf-if-buffer-read-only)
                   (unless (mark)
                     (error "The mark is not set now, so there is no region"))
                   (list (shell-command-read-minibuffer
                          "| " default-directory
                          nil nil nil 'shell-command-history)
                         (region-beginning)
                         (region-end))))
    (shell-command-on-region start end command nil 'replace))

  (defun filter-buffer (command)
    (interactive (progn
                   (barf-if-buffer-read-only)
                   (list (shell-command-read-minibuffer
                          "| " default-directory
                          nil nil nil 'shell-command-history))))
    (filter-region command (point-min) (point-max)))
  t)

;; 補完関数のコレクション部分にベクタを与えると余計なシンボルが候補に混ざるのは仕様なのかどうか
(completing-read "? " [1st 2nd 3rd 4th])
;; ベクタが obarray として扱われるのが原因か？ (obarrayp [xxx]) ;=> t
;; メモリ節約のために obarray 同士がアイテムを共有している可能性？

;; こちらは意図通り動作する
(let ((ob (obarray-make)))
  (obarray-put ob "1st")
  (obarray-put ob "2nd")
  (obarray-put ob "3rd")
  (obarray-put ob "4th")
  (completing-read "? " ob))

;; `use-package-enable-imenu-support' が有効なら必要なさそう
(defun user:enable-imenu-use-package ()
  (let ((re (rx bol "(use-package" (1+ space)
                symbol-start
                (group (1+ (or (syntax word) (syntax symbol))))
                symbol-end)))
    (add-to-list 'imenu-generic-expression (list "Package" re 1))))
(add-hook 'emacs-lisp-mode-hook 'user:enable-imenu-use-package)

;; Object#hashCode()
(sxhash 'SYMBOL) ;;=> 4132108

(eq A B)    ;;=== (= (sxhash-eq A) (sxhash-eq B))
(eql A B)   ;;=== (= (sxhash-eql A) (sxhash-eql B))
(equal A B) ;;=== (= (sxhash-equal A) (sxhash-equal B))

(with-temp-buffer
  (url-insert-file-contents
   "http://www.pitecan.com/Keisen/keisen.el")
  (eval-buffer))
(global-set-key [M-right] 'keisen-right-move)
(global-set-key [M-left]  'keisen-left-move)
(global-set-key [M-up]    'keisen-up-move)
(global-set-key [M-down]  'keisen-down-move)

;; eww on NTEmacs にて HTTPS 接続が "400 Bad Request" になる謎
(display-buffer
 (url-https (url-generic-parse-url "https://www.gnu.org/")
            (lambda (&rest args) (message "args=%S" args))
            nil))
;; url-handler はフツーに開く
(find-file "https://www.gnu.org/")

(char-width ?α)
;;=> 1 or 2

(require 'xdg)
(setf (getenv "XDG_CONFIG_HOME") (xdg-config-home)
      (getenv "XDG_CACHE_HOME") (xdg-cache-home)
      (getenv "XDG_DATA_HOME") (xdg-data-home))

(let ((with
       (lambda (parser)
         (let ((response-buffer (current-buffer)))
           (goto-char (symbol-value 'url-http-end-of-headers))
           (unwind-protect
               (funcall parser)
             (kill-buffer response-buffer))))))
  (url-retrieve "https://httpbin.org/get"
                (lambda (status)
                  (message "Status: %S" status)
                  (message "Data: %S" (funcall with #'json-read)))))
;;-> Status: (:peer (:certificate (...) :key-exchange "ECDHE-RSA" :protocol "TLS1.2" :cipher "AES-128-GCM" :mac "AEAD"))
;;-> Data: ((args) ... (url . "https://httpbin.org/get"))
;;=> #<buffer  *http httpbin.org:443*>

;; Oauth2
;; https://github.com/ifree/org-onenote/blob/master/org-onenote.el
(oauth2-auth-and-store
 "https://login.live.com/oauth20_authorize.srf"
 "https://login.live.com/oauth20_token.srf"
 "wl.offline_access offline_access office.onenote_create office.onenote_update_by_app office.onenote_update office.onenote"
 "d6c99b4d-fda1-48f0-9e0f-22103e544413"
 nil
 "https://login.live.com/oauth20_desktop.srf")
;;=> #s(oauth2-token ...)

(with-temp-buffer
  (insert-file-contents "~/tmp/xxx.el")
  (read (current-buffer)))

(global-set-key (kbd "C-x t u")
                (lambda ()
                  (interactive)
                  (setq #0=url-debug (not #0#))
                  (message "%s=%s" '#0# #0#)))

;; エラーシンボル一覧 (signal 'ERROR-SYMBOL)
(with-output-to-temp-buffer "*Errors*"
  (mapatoms
   (lambda (s)
     (let ((err (get s 'error-conditions))
           (msg (get s 'error-message)))
       (when err
         (princ (format "Error=%S,Message=%S\n" err msg)))))))

(use-package hi-lock
  :init (global-hi-lock-mode +1)
  :custom (hi-lock-file-patterns-policy t))

;; `list-colors-display' のように `defined-colors' を一覧表示
(with-current-buffer (get-buffer-create "*Colors (Hexadecimal)*")
  (save-excursion
    (let ((inhibit-read-only t))
      (erase-buffer)
      (dolist (color (mapcar 'color-complement-hex (defined-colors)))
        (insert (propertize color 'face `(:background ,color)))
        (insert "\n"))))
  (display-buffer (current-buffer)))

;; /etc/X11/rgb.txt を色付してみる
(defvar user::rgb-txt-regexp
  (rx (: bol
         (group #1=(: (* space) (group (** 1 3 num))) #1##1#) ; RGB
         (* space)
         (+? (or word space))           ; colorName
         eol)))

(defun user::rgb-txt-colorize ()
  (let ((rgb (apply #'format "#%02x%02x%02x"
                    (mapcar #'string-to-number
                            (list
                             (match-string-no-properties 2)
                             (match-string-no-properties 3)
                             (match-string-no-properties 4))))))
    (put-text-property (match-beginning 1)
                       (match-end 1)
                       'face `(:background ,rgb))))

(with-current-buffer "rgb.txt"
  (font-lock-add-keywords nil
     `((,user::rgb-txt-regexp (1 (user::rgb-txt-colorize)))))
  ;; テキストプロパティを直接変更する方法もあるがバッファが modified になる
  '(let ((inhibit-read-only t))
     (save-excursion
       (goto-char (point-min))
       (while (re-search-forward user::rgb-txt-regexp nil t)
         (user::colorize-rgb)))
     (set-buffer-modified-p nil)
     ))

;; [2019-09-27] epg.el:epg--start
;; https://emba.gnu.org/emacs/emacs/blob/emacs-26/lisp/epg.el#L647-L663
;; プロセスの標準エラー出力を `make-pipe-process' に渡しているが :coding が指定されていないため
;; gpg4win では sjis 出力が文字化けする
;; ロケールを変更する回避策はあるが万全ではない
(defun user::epg-reset-locale (f &rest args)
  (let ((process-environment
         (copy-sequence process-environment)))
    (setf (getenv "LANG") "C")
    (apply f args)))
(advice-add 'epg--start :around 'user::epg-reset-locale)

;; 連想リストをパラメータ名順でソート
(sort (frame-parameters)
      (pcase-lambda (`(,param-x . ,_) `(,param-y . ,_))
        (string< (symbol-name param-x) (symbol-name param-y))))

;; 行番号を現在行からの相対位置にする (度し難い..)
(custom-set-variables
 '(display-line-numbers-type 'relative)
 '(display-line-numbers-current-absolute nil))

;; (featurep FEATURE &optional SUBFEATURE)
(featurep 'make-network-process '(:family ipv6))
;;~> (member '(:family ipv6) (get 'make-network-process 'subfeatures))

;; scratch バッファを間違って削除しないようにロックする
(with-current-buffer "*scratch*"
  (emacs-lock-mode 'kill))

;; 似たような文字をいっしょに検索してくれる (e.g. e=é=è, o=ö, u=ü, a=å, o=ó=ö, c=ç)
;; Migemo と併用はできない
;; http://extra-vision.blogspot.com/2016/10/emacs-251-isearch.html
(setq search-default-mode #'char-fold-to-regexp)
(setq replace-char-fold t)

;; TODO: ssh-known-host-mode の font-lock が重い -> rx で書き換えてみる

;; なんでこれ定義したんだっけ‥？
(with-eval-after-load 'tar-mode
  (define-advice tar-extract (:after (&rest args))
    (when view-read-only
      (read-only-mode))))

(use-package tar-mode
  :bind (:map tar-mode-map ("f" . tar-view)))

;; フレームの作成とコマンド実行を同時に行おうとしたが
;; M-x 入力が前フレームに表示されてしまう不具合
(defun user/execute-extended-command-other-frame ()
  "Execute command (M-x) in other frame."
  (interactive)
  (make-frame)
  (call-interactively 'execute-extended-command))
(global-set-key (kbd "C-x 5 x") 'user/execute-extended-command-other-frame)

;; :smile: みたいな絵文字エイリアスを変換して入力するパッケージって既にある？
(defvar emoji-db
  (with-temp-buffer
    (url-insert-file-contents
     "https://github.com/github/gemoji/raw/master/db/emoji.json")
    (json-read)))

(seq-find (pcase-lambda ((map emoji description category aliases))
            (string= description "thumbs up"))
          emoji-db)
;;=>	((emoji . "👍")
;; 	 (description . "thumbs up")
;; 	 (category . "People & Body")
;; 	 (aliases . ["+1" "thumbsup"])
;; 	 (tags . ["approve" "ok"])
;; 	 (unicode_version . "6.0")
;; 	 (ios_version . "6.0")
;; 	 (skin_tones . t))

(setq highlight-nonselected-windows t)

;; `custom-reevaluate-setting' カスタム変数の値リセットの活用例
;; via: https://github.com/cyrus-and/dotfiles/blob/b7eb700e5d81c5ab61fb6bbfb21685073a362433/emacs/.emacs#L51-L56
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook (lambda () (custom-reevaluate-setting 'gc-cons-threshold)))

;; major-mode をフック実行の回避をしながら起動させる
;; 参考元 `custom-save-all'
(delay-mode-hooks (emacs-lisp-mode))

;; Not a Number (type-of=float)
(print 0.0e+NaN)

;; FIXME: 黒背景に濃い青は見づらい (テーマを利用していれば無用の長物)
;; (setq ansi-color-names-vector
;;  ["black" "red3" "green3" "yellow3"
;;   "royal blue"                        ; Original "blue2"
;;   "magenta3" "cyan3" "gray90"])
ansi-color-names-vector

(with-output-to-temp-buffer "*features*"
  (-each-indexed features
    (lambda (i f)
      (println (format "%03d\t%s" i f)))))

;; TODO: お役立ち正規表現いろいろ
(rx (not (char (?\x00 . ?\x7f))))       ; "[^\x00-\x7f]"
(rx (not ascii))                        ; "[^[:ascii:]]"

;; シャドーされたパスを探す
;; M-x list-load-path-shadows

;; 紛れ込んだマルチバイト文字を探す
(defun search-forward-non-ascii ()
  (interactive)
  (re-search-forward "[^[:ascii:]]" nil 'noerror))

(use-package time
  :custom
  (display-time-default-load-average nil))

;; TODO: autoload ファイルの生成と起動時に読み込ませる方法 (パッケージ開発用)
(package-generate-autoloads "unicode-escape" default-directory)
;; もしくは手動インストールさせる？
(package-install-file "FILE/dir/tar")

;; FIXME:
;; Emacs-29.4 にて loaddefs-generate が names#:autoload と相性が悪い（仕様が変わった？）
;; - ~/.emacs.d/elpa-27.1/unicode-escape-1.1/unicode-escape-autoloads.el
;;   define-namespace マクロが展開されている
;; - ~/.emacs.d/elpa-29.4/unicode-escape-1.1/unicode-escape-autoloads.el
;;   define-namespace マクロが展開されていない
