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

```emacs-lisp
;; use nonstandard .git directory (not worked yet)
;; https://emacs.stackexchange.com/q/30602
(defun magit-dotfiles ()
  (interactive)
  (let ((magit-git-global-arguments
         `("--git-dir" ,(expand-file-name "~/.dotfiles.git")
           "--work-tree" ,(expand-file-name "~")
           ,@magit-git-global-arguments)))
    (magit-status-internal "~")))

;; or
(let ((process-environment
      (copy-sequence process-environment)))
  (mapc (lambda (x) (setf (getenv (car x)) (cdr x)))
        `(("GIT_DIR" . ,(substitute-env-vars "$HOME/.dotfiles.git"))
          ("GIT_WORK_TREE" . ,(substitute-env-vars "$HOME"))))
  (magit-status-internal "~"))
```

```emacs-lisp
;; 黒背景ターミナルで文字が見えにくい場合の対応
(set-face-background 'magit-section-highlight "gray55")
```
