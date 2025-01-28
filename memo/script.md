# Emacs Lisp をスクリプトファイルとして起動する

https://www.gnu.org/software/emacs/manual/html_node/emacs/Initial-Options.html

`-batch`: わりと古くからあるオプション

```shell
emacs -batch script.el
```

`--script`: 初期化ファイル読み込みあり (`site-run-file`)
いつから追加されたっけ？

```shell
emacs --script script.el
```

`-x`: 初期化ファイル読み込みなし (`--quick` + `--script`)


```shell
#!/usr/bin/emacs -x
```

## 入出力

`--script` で実行されたスクリプトは princ, message 等の関数で出力

## テクニカルなやつ

```emacs-lisp
#!/bin/sh
:;exec emacs --quick --script "$0" -- "$@"
(pop argv) ; ignore "--"
(print argv)

;; e.g.
;; $ ./script.el HELLO World
;; Output: ("HELLO" "World")
 ```
 
ただし、引数に `--version` などが含まれると Emacs の引数処理に回されるため
`"$0" -- "$@"` と書いたほうが安全かもしれない. (当然ながら argv はズレるが)
