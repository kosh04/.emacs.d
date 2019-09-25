# Travis CI

https://travis-ci.org/

Emacs Lisp で継続インテグレーションを利用する方法

Emacs をインストールする方法は幾つかある

## パッケージからインストールする

公式が提供しているパッケージがそのまま利用可能。
オプションで ubuntu-elisp も指定しても良い。

```yaml
# .travis.yml
addons:
  apt:
    sources:
      - sourceline: 'ppa:ubuntu-elisp/ppa'
    packages:
      - emacs
      - emacs-snapshot
```

- [o] 公式バイナリなので導入が簡単
- [x] emacs-24.3 (Ubuntu 16.04 LTS) だと elisp パッケージ側がサポート対象外にしている可能性あり
- [x] ubuntu-elisp/ppa の emacs-snapshot のバージョンが微妙 (GNU Emacs 26.0.50.2)
- [x] 利用可能なバイナリが2種類しかない

## 野良ビルドする

- [o] ビルド用 Makefile の作成は比較簡単
- [o] ビルドオプションをユーザが指定できる。細かいところに手が届く
- [x] 逆に考えれば細かいところに気を使わなければいけない
- [x] 溢れ出る手書き感

Makefile は以下を参照。サブモジュールとして分離したほうが良いかもしれない

- https://github.com/kosh04/heroku-buildpack-emacs/blob/master/bin/compile.mk
- github.com/kosh04/emacs-wandbox/.ci/emacs.mk

## rejeep/evm を利用する

- https://github.com/rejeep/evm
- [Emacsのバージョンマネージャ "evm" がEmacs LispのCIにおすすめ](https://k1low.hatenablog.com/entry/2014/04/15/203856)

- [o] 今のところの一番の最適解 (ただし自分がまだ利用していないため要検証)
- [o] 提供している Emacs のバージョンが豊富
- [o] 切り替えは環境変数 `ENV_EMACS` を指定するだけ
- [o] elisp パッケージを公開しているリポジトリでよく見かける印象
