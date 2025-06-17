;;; early-init

;; Start profiler when --debug-init
(when init-file-debug
  (require 'profiler)
  (profiler-start 'cpu)
  (add-hook 'window-setup-hook #'profiler-stop 0)
  (add-hook 'window-setup-hook #'profiler-report))

;; realy useful it?
;; (setq package-enable-at-startup nil)

;; Generate cache package-quickstart.el
;; XXX: パッケージのコンパイル毎に package-quickstart.el が warning 吐きまくるの面倒くさい
;; (setq package-quickstart t)

;; バージョン間の elc 互換性を保持したい
(setq package-user-dir
      (locate-user-emacs-file
       (format "elpa-%s" emacs-major-version)))

;;(setopt frame-background-mode 'dark)

;; ? default-frame-alist
(add-to-list 'initial-frame-alist '(tool-bar-lines . 0))
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
;;(add-to-list 'default-frame-alist '(fullscreen . maximized))

(add-to-list 'initial-frame-alist '(alpha . (0.92 0.80))) ; (active . inactive)
;;(setf (frame-parameter nil 'alpha) '(0.90 0.90))
;;(setf (frame-parameter nil 'alpha) '(1.0 0.5))

;; 暗黙のリサイズを抑制すると起動時間が速くなる？
(setq frame-inhibit-implied-resize t)

(provide 'early-init)
