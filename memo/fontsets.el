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
(set-frame-font "Fira Mono-15")
(set-frame-font "Myrica M-11.5")
(set-frame-font "MyricaM M-11.5")
(set-frame-font "MigMix 1M-11")
(set-frame-font "源ノ角ゴシック Code JP R-9.0")
(set-frame-font "源真ゴシック等幅 Regular-10")
(set-frame-font "更紗等幅ゴシック J-10.5")
(set-frame-font "Source Code Pro-10.0")
(set-frame-font "Source Han Code JP-14")
(set-frame-font "Noto Sans Mono CJK JP Regular-10.0")
(set-frame-font "Unifont:pixelsize=16")
(set-frame-font "MeiryoKe_Console-11.0")
(set-frame-font "Sarasa Term J:pixelsize=16")
(set-frame-font "Sarasa Mono T J")
(set-frame-font "HackGen:pixelsize=16")

(set-fontset-font "fontset-startup" 'ascii "Unifont:pixelsize=16")

(font-info "Ricty Diminished")
(font-info (font-spec :family "Ricty Diminished" :size 16))

;;(set-face-attribute 'default nil :family "Consolas" :height 110)
;;(set-fontset-font nil 'japanese-jisx0208 (font-spec :family "MeiryoKe_Console"))
;;(setq face-font-rescale-alist '(("MeiryoKe_Console" . 1.08)))

;;  新規フォントセットの作成
(create-fontset-from-ascii-font "Unifont:pixelsize=16" nil "xxx")
(set-face-font 'default "fontset-xxx")

;; 令和合字 (U+32FF,㋿) のフォントをピンポイントに指定する
;; * 対応パッチが降ってくるまでの間に合わせ hack
(set-fontset-font t ?\u32ff "源ノ角ゴシック")
(set-fontset-font t ?\u32ff "IPAexGothic")

(defun font-monospace-p (name)
  "NAME が等幅フォント (monospace) であれば non-nil を返す."
  (if-let* ((xlfd (elt (font-info name) 0))
	    (xlfd-field (x-decompose-font-name xlfd))
	    (spacing (aref xlfd-field 9)))
      (string= spacing "m")))

(with-output-to-temp-buffer "*Font Family*"
  (dolist (family (thread-first (font-family-list)
		    (delete-dups)
		    (sort #'(lambda (x y) (string< (upcase x) (upcase y))))))
    (when (font-monospace-p family)
      (princ family)
      (terpri))))

;; その文字がどのフォントで描画されているか調べる
;; あるいは C-u C-x = (what-cursor-position) でも OK
(face-font 'default nil ?あ) ;;=> "-*-Hiragino Sans-normal-normal-normal-*-15-*-*-*-p-0-iso10646-1"
(face-font 'default nil ?🍣) ;;=> "-*-Symbola-normal-normal-semicondensed-*-15-*-*-*-p-0-iso10646-1"
(face-font 'default nil ?㋿) ;;=> "-*-IPAexGothic-normal-normal-normal-*-15-*-*-*-p-0-iso10646-1"

(setq use-default-font-for-symbols t)

(set-fontset-font nil '(#x1f000 . #x1faff) "Apple Color Emoji")

;; https://juliamono.netlify.app/
(set-fontset-font t 'unicode "JuliaMono")

;; Typographic Ligatures in Emacs
;; https://github.com/mickeynp/ligature.el
