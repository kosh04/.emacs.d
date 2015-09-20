;;; memo/xyzzy.el

;; Xyzzy の起動時処理フロー
;; http://xyzzy.s53.xrea.com/reference/wiki.cgi?p=%B5%AF%C6%B0%BB%FE%BD%E8%CD%FD%A5%D5%A5%ED%A1%BC
;; 1. $XYZZY/site-lisp/siteinit.l
;; 2. *pre-startup-hook*
;; 3. ~/.xyzzy
;; 4. *load-history-hook*
;; 5. *init-app-menus-hook*
;; 6. *command-line-mailto-hook* （-mailtoがある場合）
;; 7. *process-command-line-hook* （不明なオプションがある場合）
;; 8. *post-startup-hook*

;; Emacs の起動処理フロー
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Startup-Summary.html
;; 1. before-init-hook
;; 2. after-init-hook
;; 3. tty-setup-hook
;; 4. emacs-startup-hook
;; 5. window-setup-hook
