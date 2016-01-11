;;; memo/emacs-lisp.el

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
;; プロファイラ (profile) -> (OBSOLETE; use elp.el instead)
;; http://www.mew.org/~kazu/doc/elisp/profile.html
(require 'profiler)
(require 'elp)
(require 'disass)
(require 'debug)
;;; pp.el --- pretty printer for Emacs Lisp
(require 'pp)

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
;;; elispの依存関係を調べる
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
 10) ;;=> 3628800

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

(let (acc)
  (dolist (sym '(most-negative-fixnum
                 most-negative-short-float
                 most-negative-double-float
                 most-negative-single-float
                 most-positive-single-float
                 most-positive-long-float
                 most-positive-short-float
                 most-positive-fixnum
                 most-negative-long-float
                 most-positive-double-float))
    (if (boundp sym)
        (push (cons sym (symbol-value sym)) acc)))
  (nreverse acc))
;;=> ((most-negative-fixnum . -268435456) (most-positive-fixnum . 268435455))

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
