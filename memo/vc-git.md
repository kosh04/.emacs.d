# Version Control System (VC)

Emacsのバージョンコントロールのためのインターフェース

複数のバージョンコントロールシステムをサポートしている
(Bazzar,CVS,Git,Mercurial,Subversion など)

## 参考リンク

- Emacs25日本語マニュアル 第25章:大きなプログラムの保守

## キーバインド

- C-x v a `vc-update-change-log`
- C-x v v `vc-next-action`
- C-x v i `vc-register`  ? `git add`
- C-x v = `vc-diff`      ? `git diff` (or M-x `vc-ediff`)
- C-x v D `vc-root-diff` ? `git status -p`
- C-x v m `vc-merge`     ? `git merge`
- C-x v P `vc-push`      ? `git push`
- C-x v + `vc-update`    ? `git pull`
- C-x v ~ `vc-revision-other-window`
- C-x v g `vc-annotate`  ? `git blame`
- C-x v l `vc-print-log` ? `git log FILENAME`
- C-x v L `vc-print-root-log` ? `git log`
- C-x v I `vc-log-incoming`
- C-x v O `vc-log-outcoming`
- C-x v u `vc-revert` ? `git reset`, `git checkout FILENAME`
- C-x v G `vc-ignore`
- C-x v d `vc-dir`
- C-x v r `vc-retrieve-tag` ? `git checkout`
- C-x v s `vc-create-tag` ? `git tag`

## Magit

- https://magit.vc/
- https://magit.vc/manual/magit.html
- https://magit.vc/manual/magit-refcard.pdf
