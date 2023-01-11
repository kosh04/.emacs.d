# How to Emacs build

## 現在使用中の Emacs のビルドオプションを知りたい

```
system-configuration-options
;-> "--enable-mac-app=$HOME/Applications --prefix=$STOW_DIR/emacs-$VERSION"
```

ソースコードが残っていれば `config.status` が再利用できるかも

```shell
$ ./config.status --config
$ ./config.status --recheck
```
