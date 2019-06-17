;;; config/linux-fontset

(when window-system
  (create-fontset-from-ascii-font
   "Sarasa Term J:pixelsize=16:weight=regular:slant=normal" nil "coding")
  ;;(set-fontset-font "fontset-coding" 'unicode "Symbola" nil 'append)
  (add-to-list 'default-frame-alist '(font . "fontset-coding")))

;;(add-to-list 'initial-frame-alist '(fullscreen . maximized))
