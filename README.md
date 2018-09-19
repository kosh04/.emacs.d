[![Build Status](https://travis-ci.org/kosh04/.emacs.d.svg)](https://travis-ci.org/kosh04/.emacs.d)

## Files

- `init.el` : ここから
- `config/*.el` : 各種設定ファイル
- `memo/*` : 雑多なEmacsメモ
- `site-lisp/*.el` : 自作ライブラリいろいろ

### site-lisp

設定ファイルとは別に分離させた自作ライブラリいろいろ。

未完成な部分が多いためパッケージ登録はしていません。
MELPAにあるものは[リポジトリ参照](https://github.com/search?l=Emacs+Lisp&q=user%3Akosh04)

- `cl-compatible.el` : Common Lispのような関数と変数のまとめ (`cl-lib.el` 拡張)
- `data-uri.el` : data URI scheme を生成する (`data:image/png;base64,iVBORw0K...`)
- `gitter-irc.el` : Gitter client using IRC
- `gnome-util.el` : GNOME端末やファイルブラウザを呼び出すユーティリティ
- `google.el` : 簡易Google検索
- `m3u-mode.el` : M3U プレイリスト編集用メジャーモード
- `rust-playground.el` : Rust Playground (https://play.rust-lang.org) API クライアント
- `ssh-public-key-overlay.el` : `~/.ssh/authorized_keys` 等のBASE64鍵を見やすくする
- `textproc.el` : テキスト変換
- `xyzzy.el` : xyzzy lisp移植キットのようなもの。逆引き用ライブラリ
- `xyzzy-keymap.el` : xyzzyのキーマップを模倣する
- 他いろいろ

## Requirements

- Emacs 25 or lator
- Library
  - [init-loader](https://github.com/emacs-jp/init-loader)
  - [use-package](https://github.com/jwiegley/use-package)
- Platform
  - Windows ([NTEmacs64](https://github.com/chuntaro/NTEmacs64))
  - Linux (Debian, Ubuntu)
  - macOS (Cocoa Emacs)
- Cask (optional. for develop site-lisp)

## Installation

1. Clone this repository. (recommended `~/.emacs.d/`)
2. `cask install`
3. `make compile` (byte-compile site-lisp/*.el)

## Rules

- デフォルト設定を利用する (覚えることが少ない方がよい)
- ライブラリが外部プロセスに依存する場合は PATH を通しておく (特に NTEmacs)
- パッケージはなるべく安定版を優先してインストールしたい (gnu, melpa-stable)
- Emacs と xyzzy の操作感を共存させる

### init-loader (config/nn-xxx.el) の読み込み順序

- 10. パッケージなど優先的に設定すべきもの
- 20. 組み込みライブラリの基本設定
- 30. 外部ライブラリの設定 (優先度:高)
- 50. 外部ライブラリの設定 (優先度:中)
- 60. 自作ライブラリの設定 (他の設定に依存させない)

## TODO

- Better Directory Structure (refer to "Filesystem Hierarchy Standard")
