;;; config/GDB -- GDB/MI migration

(use-package gdb-mi
  :custom
  (gdb-show-main t)
  (gdb-many-windows t)
  ;; non-nil ならばスクリプトのディレクトリでデバッグ実行する
  ;; e.g. git-svn コマンド (perl/pdb) は実行時のディレクトリにリポジトリが存在するため移動してほしくない
  (gud-chdir-before-run nil)
  ;;(setq gdb-use-separate-io-buffer t)
  ;;(setq gud-tooltip-echo-area t)
  :hook
  (gdb-mode . gud-tooltip-mode)
  (gdb-mode . tool-bar-mode))

(use-package gud-lldb
  :commands (lldb)
  :load-path "site-lisp/_vendor")
