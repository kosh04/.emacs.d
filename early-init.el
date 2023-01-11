;;; early-init

;; Start profiler when --debug-init
(when init-file-debug
  (require 'profiler)
  (profiler-start 'cpu)
  (add-hook 'window-setup-hook #'profiler-stop 0)
  (add-hook 'window-setup-hook #'profiler-report))

;; realy useful it?
(setq package-enable-at-startup nil)

;; Generate cache package-quickstart.el
;; XXX: パッケージのコンパイル毎に package-quickstart.el が warning 吐きまくるの面倒くさい
(setq package-quickstart nil)

;; ? default-frame-alist
(add-to-list 'initial-frame-alist '(menu-bar-lines . 0))
(add-to-list 'initial-frame-alist '(tool-bar-lines . 0))

(provide 'early-init)
