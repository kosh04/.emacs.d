# memo/Input-Method --- 日本語入力について

https://www.gnu.org/software/emacs/manual/html_node/emacs/Input-Methods.html

動機: 偶に NTEmacs の IME が効かなくなる (`<kanji>`) ことがあるので、Emacs デフォルトの入力方式を覚えておくと保険になるかもしれない。

## 設定

    (setq quail-japanese-use-double-n t)

## キーワード

- <kbd>C-h I</kbd> : M-x describe-input-method
- `(info "(emacs) Input Methods")`
  see also file:input-method-ja.txt
- `lisp/international/quail.el` --- provides simple input method for multilingual text
- `lisp/leim/quail/japanese.el`
  「ローマ字入力及び仮名漢字変換による日本語入力メソッド」という日本語コメントが記載されている
- leim: libraries of Emacs Input Methods

## キーバインディング

- <kbd>C-h</kbd> : ヘルプバッファを表示
- <kbd>C-i</kbd> : 文節を一文字短くする
- <kbd>C-o</kbd> : 文節を一文字長くする
- <kbd>C-f</kbd> : 次の文節ヘ移動する
- <kbd>K</kbd> : ひらがな⇔カタカナ変換
