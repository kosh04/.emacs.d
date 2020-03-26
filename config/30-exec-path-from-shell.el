;; config/exec-path-from-shell

;; NOTE: Dock から Emacs アプリを起動するとシェルの環境変数が引き継がれない
;; [2019-05-19] 現在は LaunchAgents + launchctl setenv にて対策済
;; 詳細は以下のファイルを参照
;; * ~/Library/LaunchAgents/user.environ.plist
;; * ~/.local/setenv.sh

(use-package exec-path-from-shell
  :disabled
  :demand t
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "PS1")
  (exec-path-from-shell-copy-env "GOPATH")
  )
