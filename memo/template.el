;;; memo/template

;; 定型文 / テンプレート

;; - autoinsert
;; - yasnippet

;; 新規ファイル作成時にテンプレートを挿入してくれる
;; C/C++, Makefile, HTML, Tex, Man, Elisp, Texi, etc...
(auto-insert-mode +1)

;; - skeleton.el テンプレートを挿入する
(require 'skeleton)

(define-skeleton html-headline-1
  "HTML level 1 headline tags."
  nil
  "<h1>" _ "</h1>")

;; tempo.el --- Flexible template insertion

;; abbrev の展開にテンプレートを指定することも可能らしい
(define-abbrev global-abbrev-table "sample" "" 'html-headline-1)
