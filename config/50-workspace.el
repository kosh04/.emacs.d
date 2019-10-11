;;; config/workspace

;; workspace / window-configuration manager
;; see also config/50-tabbar.el

(use-package eyebrowse
  :bind
  ("<C-tab>"   . eyebrowse-next-window-config)
  ("<C-S-tab>" . eyebrowse-prev-window-config)
  :custom
  (eyebrowse-wrap-around t)
  :custom-face
  (eyebrowse-mode-line-active
   ((t (:inherit highlight))))
  )
