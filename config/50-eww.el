;;; config/eww.el

;; w3m.el のキーバインドとの互換性は特にない

;; TODO:
;;   - [ ] 文字コードの自動判別 `eww-display-html'

;; KeyBinding
;; - M-s M-w `eww-search-words'
;; ***
;; - g `eww-reload'
;; - n `eww-next-url' (rel="next/prev/up" を行き来する)
;; - p `eww-previous-url'
;; - u `eww-up-url'
;; - l `eww-back-url'
;; - r `eww-forward-url'
;; - H `eww-list-histories'
;; - b `eww-add-bookmark'
;; - B `eww-list-bookmarks'
;; - & `eww-browse-with-external-browser'
;; - w `eww-copy-page-url'
;; - C `url-cookie-list'
;; - d `eww-download' (先頭にヘッダーが付属するのは仕様？:emacs-26.1にて修正済)
;; - E `eww-set-character-encoding' (文字コードの指定)
;; - q `quit-window'
;; &rest (help-xref-interned 'eww-mode-map)

(defun user:eww-send-to-google-translate ()
  "EWWで閲覧中のウェブページを外部ブラウザで翻訳する."
  (interactive)
  (unless (eww-current-url)
    (error "not in EWW"))
  (let ((url (concat "http://translate.google.com/translate?hl=ja&sl=auto&tl=ja&u="
                     (url-hexify-string (eww-current-url)))))
    (eww-browse-with-external-browser url)))

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
             ("Q" . kill-this-buffer)
             ("e" . user:eww-send-to-google-translate)
             ))
