## Files

```
* site-lisp/          - 自作ライブラリいろいろ
* +- cl-compatible.el - Common Lispのような関数と変数のまとめ
* +- gnome-util.el    - GNOME端末やファイルブラウザを呼び出すユーティリティ
* +- google.el        - 簡易Google検索
* +- xyzzy.el         - xyzzy lisp移植キットのようなもの。逆引き用ライブラリ
* +- xyzzy-keymap.el  - xyzzyのキーマップを模倣する
* config/xxx.el       - 各種設定ファイル
* memo/xxx.el         - 雑多なEmacsLispメモ
```

## 動作環境

Emacs 24 以上

## ルール

- なるべく、各ライブラリのデフォルト設定を利用する
- Emacs と xyzzy の操作感を共存させたい

## Usage

```elisp
(add-to-list 'load-path "~/path/to/emacs-lisp/" t)
(load "config/package")
(load "site-lisp/cl-compatible")
(load "site-lisp/xyzzy")
(load "site-lisp/misc")
(load "config/backup")
(load "config/keymaps")
...
```
