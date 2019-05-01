;;; memo/fontsets.el

;; M-x list-fontsets
;; M-x describe-font
;; M-x describe-fontset

;; http://ja.wikipedia.org/wiki/XLFD

;; フォント切り替え
(set-frame-font
   "-outline-ＭＳ ゴシック-normal-r-normal-normal-16-120-96-96-c-*-iso8859-1"
   t)

;; M-x menu-set-font

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

(set-frame-font "Menlo")
(set-frame-font "Hack")
(set-frame-font "Monaco")
(set-frame-font "DejaVu Sans Mono")

;; 123456789012345678901234567890
;; ABCDEFGHIJKLMNOPQRSTUVWXYZabcd
;; あいうえおかきくけこさしすせそ
;; ◎○●▲■◎○●▲■◎○●▲■
;; ×÷±＋−×÷±＋−×÷±＋−
;; 123456789012345678901234567890
;; ΑΒΓαβγΑΒΓαβγΑΒΓαβγΑΒΓαβγΑΒΓαβγ
