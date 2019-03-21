;; config/exec-path-from-shell

;; NOTE: EmacsForMacOSX をアプリケーションとして起動すると /usr/local/bin がパスに含まれない

(use-package exec-path-from-shell
  :defer nil ;; disable use-package-always-defer=t
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "PS1")
  (exec-path-from-shell-copy-env "GOPATH")
  )
