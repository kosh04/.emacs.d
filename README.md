[![Build Status](https://travis-ci.org/kosh04/.emacs.d.svg)](https://travis-ci.org/kosh04/.emacs.d)

## Files

- `init.el` : ここから
- `config/*.el` : 各種設定ファイル
- `memo/*` : 雑多なEmacsメモ
- `site-lisp/*.el` : 自作ライブラリいろいろ

### site-lisp

設定ファイルとは別に分離させた自作ライブラリ

- `cl-compatible.el` : Common Lispのような関数と変数のまとめ (`cl.el` 拡張)
- `gnome-util.el` : GNOME端末やファイルブラウザを呼び出すユーティリティ
- `google.el` : 簡易Google検索
- `unicode-escape.el` : Unicodeエスケープ文字列の変換と復元
- `xyzzy.el` : xyzzy lisp移植キットのようなもの。逆引き用ライブラリ
- `xyzzy-keymap.el` : xyzzyのキーマップを模倣する

MELPAに登録してあるものは[リポジトリ参照](https://github.com/search?l=Emacs+Lisp&q=user%3Akosh04)

## Requirements

- Emacs 24 or lator
- Library
  - [init-loader](https://github.com/emacs-jp/init-loader)
  - [use-package](https://github.com/jwiegley/use-package)
- Platform
  - Windows ([NTEmacs64](https://github.com/chuntaro/NTEmacs64))
  - OS X (Cocoa Emacs)
- Cask (optional. for develop site-lisp)

## Installation

1. Clone this repository. (recommended `~/.emacs.d/`)
2. `cask install`
3. `make compile` (byte-compile site-lisp/*.el)

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
