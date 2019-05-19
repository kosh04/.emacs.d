;;; memo/newlisp.el

(add-to-list 'load-path "~/src/gitrepo/newlisp-mode")
(load "~/src/gitrepo/newlisp-mode/newlisp-mode.el")

(require 'make-mode)
(add-to-list 'auto-mode-alist '("makefile_.*" . makefile-mode))

;; newLISPのソースコードのインデント
(defun newlisp-c-mode ()
  (interactive)
  (c-mode)
  (c-set-style "whitesmith") ; ブロック{}と内部のステートメントインデントが一致する
  (setq indent-tabs-mode t)
  (setq c-indent-level 8)
  (setq c-basic-offset 8)
  (setq tab-width 8)
  (c-set-offset 'defun-open 0)
  (c-set-offset 'defun-close 0)
  (c-set-offset 'defun-block-intro 0)
  ;(c-set-offset 'brace-entry-open 2)
  ;; (c-set-offset 'case-label '+)
  (c-set-offset 'substatement-open 8)
  ;; (setq fill-column 75)
  )
(add-to-list 'auto-mode-alist '("newlisp/*\\.[ch]$" . newlisp-c-mode))

;; ac-newlisp
(load "~/src/gitrepo/ac-newlisp/ac-newlisp.el")
(add-to-list 'ac-modes 'newlisp-mode)
(add-to-list 'newlisp-mode-hook 'ac-newlisp-setup)
(global-auto-complete-mode)

(defun run-newlisp ()
  (interactive)
  ;; run-scheme?
  (let ((default-process-coding-system '(utf-8 . utf-8)))
    (run-lisp (format "C:/PROGRA~1/newlisp/newlisp.exe -C -w %s"
                      (expand-file-name default-directory)))))

;;; ##TCP接続でnewLISP
(defvar nl-process nil)
(defun newlisp-process ()
  (or (and (processp nl-process)
           (eq 'open (process-status nl-process))
           nl-process)
      (let ((default-process-coding-system newlisp-process-coding-system)
            (buffer (get-buffer-create "*newLISP for TCP/IP*")))
        (call-process "touch" nil nil nil "/tmp/nl-daemon.log")
        (call-process "newlisp" nil 0 nil "-C" "-L" "/tmp/nl-daemon.log" "-p" "4711")
        (setq nl-process
              (open-network-stream "newlisp" buffer "localhost" 4711))
        nl-process)))

;; newlisp-generic-mode
(require 'generic-x)
(defvar newlisp-keywords
  (car (read-from-string
        (shell-command-to-string
         (format "newlisp -n -e \"%s\""
                 '(map term (filter (fn (s) (primitive? (eval s))) (symbols MAIN))))))))

(define-generic-mode newlisp-generic-mode
    '(?\; ?\#)
  newlisp-keywords
  `(("\\b\\(-?[0-9]+\\|-?[0-9]+\\.[0-9]+\\|-?0x[0-9a-fA-F]+\\)\\b"
     . font-lock-variable-name-face)
    )
  '("\\.lsp$")
  nil )

(defun run-newlisp-sjis ()
  (interactive)
  (let ((newlisp-command "newlisp_sjis")
        (newlisp-process-coding-system 'sjis)
        (default-process-coding-system 'sjis))
    (run-newlisp)))

(defun newlisp-browse-manual-w3m ()
       (interactive)
       (w3m-find-file newlisp-manual-html))

(add-hook 'newlisp-mode-hook
          #'(lambda ()
              (setq comment-start "; ")
              ;; あまり好まないがタブ幅4のファイルがあまりに多いので
              (setq tab-width 4)
              ))

;; [2014-09-16] test for newlisp-mode syntax color
(custom-set-faces
 '(default ((t (:background "black" :foreground "#55FF55")))))
