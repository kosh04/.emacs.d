;;; config/linux-conf

(when (and window-system (font-info "Noto Color Emoji"))
  (set-fontset-font nil '(#x1F000 . #x1FAFF) "Noto Color Emoji"))
