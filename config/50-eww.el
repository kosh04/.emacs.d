;;; config/eww.el

;; w3m.el のキーバインドとの互換性は特にない

;; KeyBinding
;; n - eww-next-url
;; p - eww-previous-url
;; l - eww-back-url
;; r - eww-forward-url
;; H - eww-list-histories
;; b - eww-add-bookmark
;; B - eww-list-bookmarks
;; & - eww-browse-with-external-browser
;; w - eww-copy-page-url
;; C - url-cookie-list
;; d - eww-download (先頭にヘッダーが付属するのは仕様？)
;; q - quit-window
;; &rest (help-xref-interned 'eww-mode-map)

;; (set qeww-search-prefix "http://www.google.co.jp/search?q=")

(use-package eww
  :defer t
  :bind (("=" . eww-view-source)
         ("[" . eww-back-url)
         ("]" . eww-forward-url)
         ("<M-left>" . eww-back-url)
         ("<M-right>" . eww-forward-url)
         ("<backspace>" . eww-back-url)
         ("Q" . kill-this-buffer)))
