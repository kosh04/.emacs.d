;;; .trash.el --- バージョンアップ等により不要になったコード

;; https://github.com/tkf/emacs-request/issues/111
;; v0.3.2 にて解決済み
(require 'request)
(when (and (eq system-type 'windows-nt)
           (file-exists-p "C:\\WINDOWS\\system32\\curl.exe"))
  (advice-add 'request--curl-command
              :filter-return
              (lambda (x)
                "curl.exe since Windows 10 RS4 (1803) dosn't support --compressed option."
                (delete "--compressed" x))))

;; 自動終了してくれないので一時的な対策 [2015-08-31]
(when (eq system-type 'windows-nt)
  (defun user:ispell-killall ()
    (ispell-kill-ispell t))
  (add-hook 'kill-emacs-hook 'user:ispell-killall))

;; available in Emacs24.4+
(or (fboundp 'with-eval-after-load)
    (defmacro with-eval-after-load (feature &rest body)
      "Execute BODY after FEATURE is loaded."
      (declare (indent 1) (debug t))
      `(eval-after-load ,feature
         (lambda () ,@body))))
