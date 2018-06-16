;;; config/GDB

(with-eval-after-load 'gdb-mi
  (custom-set-variables
   '(gdb-show-main t)
   '(gdb-many-windows t)
   ;; non-nil ならばスクリプトのディレクトリでデバッグ実行する
   ;; e.g. git-svn コマンド (perl/pdb) は実行時のディレクトリにリポジトリが存在するため移動してほしくない
   '(gud-chdir-before-run nil)
   )
  ;;(setq gdb-use-separate-io-buffer t)
  ;;(setq gud-tooltip-echo-area t)
  (add-hook 'gdb-mode-hook #'gud-tooltip-mode))

(use-package gud-lldb
  :load-path "~/.emacs.d/vendor")
