;;; memo/server.el

(require 'server)

(server-start)

;; http://www.emacswiki.org/cgi-bin/wiki/EmacsClient
server-mode
server-visit-hook
server-done-hook
server-window
server-process
(process-status server-process)         ; listen / closed

;; emacsclient
;; http://www.emacswiki.org/cgi-bin/wiki/EmacsClient
(expand-file-name "emacs.bash" data-directory)

(add-hook 'server-visit-hook
          (defun topmost ()
            (frame-first-window)
            (x-focus-frame (selected-frame))
            (frame-focus (selected-frame))))

;; emacsclientを使って動的にサーバの状態を変更する
(server-mode 1)
;; $ emacsclient -e '(message "Hello!")'

;; *** `server-eval-at' の仕組み
;; (server-eval-at SERVER FORM)

;; $ emacsclient -f SERVER -e FORM とだいたい同じ？
(server-eval-at "server" '(emacs-version))
;;=> "GNU Emacs 24.5.1 ..."

;; サーバ名 SERVER -> ~/.emacs.d/server/SERVER

;; $ cat ~/.emacs.d/server/server
;; 127.0.0.1:51102 8612
;; h|d:]k,Y+Jc~1Yw[3(fduO"J\2bJz51&Kyiuf'pjYXsl/81k/Co]/'M?'E*G*<v)

;; $ cat <<EOF
;; -auth $(sed -n 2p ~/.emacs.d/server/server)
;; -eval (emacs&-version)
;; EOF | netcat localhost 51102
;; -emacs-pid 8612
;; -print "GNU&_Emacs&_24.5.1&_..."

(server-unquote-arg "GNU&_Emacs&_24.5.1&_...")
