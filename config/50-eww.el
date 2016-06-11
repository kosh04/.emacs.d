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

;; (setq eww-search-prefix "http://www.google.co.jp/search?q=")

(use-package eww
  :defer t
  :config
  (defun user:eww-show-url-at-point ()
    (get-text-property (point) 'shr-url))
  (defun user:eww-setup ()
    (setq-local eldoc-documentation-function #'user:eww-show-url-at-point)
    (eldoc-mode))

  (add-hook 'eww-mode-hook 'user:eww-setup)

  (bind-keys :map eww-mode-map
             ("=" . eww-view-source)
             ("[" . eww-back-url)
             ("]" . eww-forward-url)
             ("<M-left>" . eww-back-url)
             ("<M-right>" . eww-forward-url)
             ("<backspace>" . eww-back-url)
             ("C-k" . eww)
             ("Q" . kill-this-buffer)))
