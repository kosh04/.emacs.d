[![Build Status](https://travis-ci.org/kosh04/emacs-lisp.svg)](https://travis-ci.org/kosh04/emacs-lisp)

## Files

```
+- init.el              - ここから
+- config/xxx.el        - 各種設定ファイル
+- memo/xxx.el          - 雑多なEmacsLispメモ
`- site-lisp/           - 自作ライブラリいろいろ
   +- cl-compatible.el  - Common Lispのような関数と変数のまとめ
   +- gnome-util.el     - GNOME端末やファイルブラウザを呼び出すユーティリティ
   +- google.el         - 簡易Google検索
   +- unicode-escape.el - Unicodeエスケープ文字列のエンコードとデコード
   +- xyzzy.el          - xyzzy lisp移植キットのようなもの。逆引き用ライブラリ
   `- xyzzy-keymap.el   - xyzzyのキーマップを模倣する
```

## Requirements

- Emacs 24 or lator
- [init-loader](https://github.com/emacs-jp/init-loader)
- [use-package](https://github.com/jwiegley/use-package)

## Installation

1. Clone thie repository. (recommended `~/.emacs.d/`)
2. Put `(load "/path/to/init.el" t)` into your `.eamcs`.

## Rules

- デフォルト設定を利用する (覚えることが少ない方がよい)
- ライブラリが外部プロセスに依存する場合は `PATH` を通しておく (特に NTEmacs)
- Emacs と xyzzy の操作感を共存させる
- `config/nn-xxx.el` の番号付けルール一覧

```
10. パッケージなど優先的に設定すべきもの
20. 組み込みライブラリの基本設定
50. 外部ライブラリの設定
60. 自作ライブラリの設定 (他の設定に依存させない)
```
