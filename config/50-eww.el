;;; config/eww.el

;; w3m.el のキーバインドとの互換性は特にない

;; KeyBinding
;; n - eww-next-url (rel="next/prev/up" を行き来する)
;; p - eww-previous-url
;; u - eww-up-url
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

(use-package eww
  :defer t
  :config
  (require 's)
  (defun user:eww-show-url-at-point ()
    (let ((url (get-text-property (point) 'shr-url))
          (width (- (window-width) 3)))
      (s-truncate width url)))
  (defun user:eww-setup ()
    (setq-local eldoc-documentation-function #'user:eww-show-url-at-point)
    (eldoc-mode))

  (add-hook 'eww-mode-hook 'user:eww-setup)

  (defun user:eww-readable ()
    "現在閲覧中のウェブページを読みやすくする."
    (interactive)
    (cl-assert (eww-current-url))
    (let* ((url (format "https://outlineapi.com/parse_article?source_url=%s"
                        (url-hexify-string (eww-current-url))))
           (html (cdr (assoc 'html (assoc 'data (json-read-file url)))))
           (dom (with-temp-buffer
                  (insert html)
                  (libxml-parse-html-region (point-min) (point-max)))))
      (eww-save-history)
      (eww-display-html nil nil dom nil (current-buffer))))

  ;; (setq eww-search-prefix "http://www.google.co.jp/search?q=")

  (bind-keys :map eww-mode-map
             ("=" . eww-view-source)
             ("[" . eww-back-url)
             ("]" . eww-forward-url)
             ;;("j" . forward-line)       ; hjkl
             ;;("k" . previous-line)
             ("j" . (lambda (&optional arg) (interactive "P") (scroll-up   (or arg 1))))
             ("k" . (lambda (&optional arg) (interactive "P") (scroll-down (or arg 1))))
             ("<M-left>"  . eww-back-url)
             ("<M-right>" . eww-forward-url)
             ("C-k" . eww)
             ("R" . user:eww-readable)
             ("Q" . kill-this-buffer)))
