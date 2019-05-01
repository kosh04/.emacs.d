;;; memo/fontsets.el

;; M-x list-fontsets

;; http://ja.wikipedia.org/wiki/XLFD
;; http://extra-vision.blogspot.com/2016/07/emacs.html
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Low_002dLevel-Font.html
;; https://ayatakesi.github.io/emacs/25.1/Fonts.html
;; [Emacs のフォント設定を克服する](http://extra-vision.blogspot.com/2016/07/emacs.html)

;; フォント切り替え
(set-frame-font
   "-outline-ＭＳ ゴシック-normal-r-normal-normal-16-120-96-96-c-*-iso8859-1"
   t)

;; 元ネタ: http://www.gnu.org/software/emacs/windows/Fonts-and-text-translation.html
(defun list-complete-fonts ()
  "フォント一覧"
  (interactive)
  (with-output-to-temp-buffer "*List Fonts*"
    (dolist (font (x-list-fonts "*"))   ; or (w32-select-font)
      (princ font)
      (terpri))))

(with-output-to-temp-buffer "*Font Family*"
  (dolist (font (font-family-list))
    (princ font)
    (terpri)))
