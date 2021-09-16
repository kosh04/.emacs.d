;;; early-init

;; realy useful it?
(setq package-enable-at-startup nil)

;; Generate cache package-quickstart.el
(setq package-quickstart t)

;; ? default-frame-alist
(add-to-list 'initial-frame-alist '(menu-bar-lines . 0))
(add-to-list 'initial-frame-alist '(tool-bar-lines . 0))

(provide 'early-init)
