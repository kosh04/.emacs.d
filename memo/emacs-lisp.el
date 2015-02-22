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
;; a source-level debugger for Emacs Lisp
(require 'edebug)
;;; pp.el --- pretty printer for Emacs Lisp
(require 'pp)

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

(add-to-list 'minor-mode-alist '(debug-on-error " (ﾟдﾟ)"))
(toggle-debug-on-error)

(abort-recursive-edit)
(exit-recursive-edit)
(quit-recursive-edit)                   ; alias for `abort-recursive-edit'

;;; @@compile
;; 指定したディレクトリ以下を再バイトコンパイル
(byte-recompile-directory "~/lib/emacs/" t) ; *.elcのないファイルも強制的に？

;; C-u C-M-x (eval-defun) で edebug 起動
