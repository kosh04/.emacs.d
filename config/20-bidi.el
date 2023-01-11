;; config/Bidirectional-Display

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Bidirectional-Editing.html
;; https://ayatakesi.github.io/lispref/25.3/html/Bidirectional-Display.html

;; Disable all (but not supported)
(setq bidi-inhibit-bpa t)
(setq bidi-display-reordering nil)
(setq bidi-paragraph-direction 'left-to-right)
