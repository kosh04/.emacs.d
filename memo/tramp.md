memo/Tramp
----------

## Link

- https://www.gnu.org/software/tramp/
- https://www.emacswiki.org/emacs/TrampMode
- http://wikemacs.org/wiki/TRAMP
- `(info "(tramp)")`


## Syntax

    /method:user@host#port:filename

省略された箇所はデフォルト値が利用される

- `tramp-default-host`
- `tramp-default-host-alist`
- `tramp-default-method`
- `tramp-default-method-alist`
- `tramp-default-proxies-alist`
- `tramp-default-user`
- `tramp-default-user-alist`


## Example

一番簡単な呼び出し方。
ホスト名、ユーザ名等は `ssh/config` を考慮してくれる (plink では利用不可)

    /example.com:~/

ユーザ名、ポート番号を指定する

    /USER@localhost#2222:/etc/hosts

ssh 経由で開く (デフォルトでは `scp` / `pscp` つまりファイルコピーを利用する)

    /ssh:localhost:/bin/sh

sudo でファイルを開く

    /sudo::/path/to/file

    /sudo:USER@localhost:/path/to/file

## デバッグ

```emacs-lisp
(custom-set-variables
 '(tramp-use-ssh-controlmaster-options nil)
 '(tramp-verbose 4))
```
