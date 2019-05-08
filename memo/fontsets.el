;;; memo/fontsets.el

;; 123456789012345678901234567890
;; ABCDEFGHIJKLMNOPQRSTUVWXYZabcd
;; あいうえおかきくけこさしすせそ
;; ◎○●▲■◎○●▲■◎○●▲■
;; ×÷±＋−×÷±＋−×÷±＋−
;; 123456789012345678901234567890
;; ΑΒΓαβγΑΒΓαβγΑΒΓαβγΑΒΓαβγΑΒΓαβγ

;; abcdefg hijklmn opqrstu vwxyz
;; ABCDEFG HIJKLMN OPQRSTU VWXYZ
;; 0123456789
;; あかさたなはまやらわ
;; アカサタナハマヤラワ
;; 零壱弐参肆伍陸漆捌玖拾

;; (w32-select-font)
;; (x-select-font)
;; M-x menu-set-font
;; M-x list-fontsets
;; M-x describe-font
;; M-x describe-fontset

;; http://ja.wikipedia.org/wiki/XLFD
;; http://extra-vision.blogspot.com/2016/07/emacs.html
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Low_002dLevel-Font.html
;; https://ayatakesi.github.io/emacs/25.1/Fonts.html
;; [Emacs のフォント設定を克服する](http://extra-vision.blogspot.com/2016/07/emacs.html)

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

(set-frame-font "Consolas-10.5")
(set-frame-font "Menlo")
(set-frame-font "Hack-10.0")
(set-frame-font "Monaco")
(set-frame-font "DejaVu Sans Mono-10.0")
(set-frame-font "Inconsolata-10.5")
(set-frame-font "Ricty Diminished-10.5")
(set-frame-font "Fira Code-10.0")
(set-frame-font "Myrica M-11.5")
(set-frame-font "MyricaM M-11.5")
(set-frame-font "MigMix 1M-11")
(set-frame-font "源ノ角ゴシック Code JP R-9.0")
(set-frame-font "源真ゴシック等幅 Regular-10")
(set-frame-font "Source Code Pro-10.0")
(set-frame-font "Noto Sans Mono CJK JP Regular-10.0")
(set-frame-font "Unifont:pixelsize=16")
(set-frame-font "MeiryoKe_Console-11.0")

(set-fontset-font "fontset-startup" 'ascii "Unifont:pixelsize=16")

(font-info "Ricty Diminished")
(font-info (font-spec :family "Ricty Diminished" :size 16))

;;(set-face-attribute 'default nil :family "Consolas" :height 110)
;;(set-fontset-font nil 'japanese-jisx0208 (font-spec :family "MeiryoKe_Console"))
;;(setq face-font-rescale-alist '(("MeiryoKe_Console" . 1.08)))


;; 令和合字 (U+32FF,㋿) のフォントをピンポイントに指定する
;; * 対応パッチが降ってくるまでの間に合わせ hack
(set-fontset-font t ?\u32ff "源ノ角ゴシック")
(set-fontset-font t ?\u32ff "IPAexGothic")
