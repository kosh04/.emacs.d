## File List

```
+- site-lisp/           - 自作ライブラリいろいろ
|  +- cl-compatible.el  - Common Lispのような関数と変数のまとめ
|  +- gnome-util.el     - GNOME端末やファイルブラウザを呼び出すユーティリティ
|  +- google.el         - 簡易Google検索
|  +- unicode-escape.el - Unicodeエスケープ文字列のエンコードとデコード
|  +- xyzzy.el          - xyzzy lisp移植キットのようなもの。逆引き用ライブラリ
|  +- xyzzy-keymap.el   - xyzzyのキーマップを模倣する
+- config/xxx.el        - 各種設定ファイル
+- memo/xxx.el          - 雑多なEmacsLispメモ
```

## Dependencies

- Emacs 24 or lator
- use-package.el

## Coding Rules

- なるべく、各ライブラリのデフォルト設定を利用する
- Emacs と xyzzy の操作感を共存させたい
