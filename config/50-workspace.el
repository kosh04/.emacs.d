;;; config/workspace

;; workspace, window-configuration manager
;; see also config/50-tabbar.el

(use-package eyebrowse
  :init (eyebrowse-mode t)
  :custom
  (eyebrowse-wrap-around t)
  :custom-face
  (eyebrowse-mode-line-active
   ((t (:inherit highlight))))
  )
