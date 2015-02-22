### Files

* .emacs.my.el      - 設定ファイル (old)
* site-lisp/        - 自作ライブラリいろいろ
* - gnome-util.el   - GNOME端末やファイルブラウザを呼び出すユーティリティ
* - google.el       - 簡易Google検索
* - xyzzy.el        - xyzzylisp移植キットのようなもの。逆引き用ライブラリ
* - xyzzy-keymap.el - xyzzyのキーマップを模倣する
* config/xxx.el     - 各種設定
* memo/xxx.el       - 雑多なEmacsLispメモ


## Usage

```elisp
(add-to-list 'load-path "~/path/to/emacs-lisp/" t)
(load "config/backup")
(load "site-lisp/misc")
```

ほとんど自分用メモばかりです。
