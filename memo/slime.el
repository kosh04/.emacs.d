;;; memo/slime.el

;;
(defun run-slime (&optional modern)
  (interactive "P")
  (shell-command
   (format "%s -K full -q -ansi %s -i %s &"
           (expand-file-name "bin/clisp.exe" clisp-directory)
           ;; inferior-lisp-program
           (if modern "-modern" "")
           (expand-file-name ".slime.lisp" clisp-directory)))
  (sleep-for 0.5)
  (slime-connect "127.0.0.1" 4005))

;;; CLISPごった煮2.46の設定
(progn
(add-to-list 'load-path "c:/usr/local/clisp-2.46-full/lib/slime")
(load-library "slime")
(slime-setup '(slime-scratch slime-fancy inferior-slime))
(setq slime-net-coding-system 'utf-8-unix) ; 日本語を使いたい
(setq slime-lisp-implementations
      '((clisp ("C:/usr/local/clisp-2.44/clisp.exe" "-K full"
                "-Efile utf-8" "-on-error debug" "-I"))
        (olio ("c:/usr/local/clisp-2.46-full/bin/clisp.exe")
         :init;; slime-olio-init-command
         slime-init-command
         )))
(setq slime-default-lisp 'olio)
(global-set-key [(control ?c) (control ?z)] 'slime-repl)
(define-key slime-repl-mode-map [(control ?c) (control ?c)] 'slime-quit-lisp)
(require 'hyperspec)
(global-set-key "\C-cH" 'hyperspec-lookup)
;; CLISP ごった煮
(load-library "clisp-olio")
)

;;; SLIME Tips
;;; http://www.cliki.net/SLIME%20Tips
;; Connecting automatically
(defun cliki:start-slime ()
  (unless (slime-connected-p)
    (save-excursion (slime))))

(add-hook 'slime-mode-hook 'cliki:start-slime)

;; SLIME 終了後に Fuzzy buffer が消せないバグ
;; ~/src/slime/contrib/slime-fuzzy.el
(defun slime-fuzzy-done ()
  (when slime-fuzzy-target-buffer
    ;; 削除されたバッファをセットしようとするとエラー
    (set-buffer slime-fuzzy-target-buffer)
    ...))

(defadvice slime-space (around enable-anty-insert activate)
  ;; FIXME
  (if (and anthy-minor-mode
           (or (not (string-equal anthy-preedit ""))
               anthy-enum-candidate-p
               anthy-enum-rcandidate-p))
      (anthy-insert)
      ad-do-it))
