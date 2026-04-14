;;; memo/coding-systems,charset,unicode

;;; 非ASCII文字 -- GNU Emacs Lispリファレンスマニュアル
;;; http://www.bookshelf.jp/texi/elisp-manual/21-2-8/jp/elisp_33.html

;; エンコード関数・変数 たぶんまだある
;; mule{,cmds}.el
mule-keymap
universal-coding-system-argument (C-x RET c)
set-buffer-file-coding-system    (C-x RET f)
set-file-name-coding-system      (C-x RET F)
set-buffer-process-coding-system (C-x RET p)
revert-buffer-with-coding-system (C-x RET r)
set-terminal-coding-system       (C-x RET t)
set-keyboard-coding-system       (C-x RET k)

coding-system-for-read                  ; nil
coding-system-for-write                 ; nil
locale-coding-system                    ; cp932

(apropos "default-.*-coding-system")
default-buffer-file-coding-system       ; japanese-shift-jis
default-file-name-coding-system         ; japanese-shift-jis
default-keyboard-coding-system          ; japanese-shift-jis
default-process-coding-system           ; (japanese-shift-jis-dos . japanese-shift-jis-unix)
default-sendmail-coding-system          ; iso-2022-jp
default-terminal-coding-system          ; japanese-shift-jis
(default-value 'buffer-file-coding-system) ; japanese-shift-jis

(modify-coding-system-alist 'process "*" 'utf-8)

;; 文字列からcharsetの判定方法は？ (intern string) でなくて
coding-system-alist

(coding-system-base 'utf-8)                  ; utf-8 (or mule-utf-8)

;; 文字コード判定
(detect-coding-string "こんにちは")          ; (japanese-shift-jis iso-2022-jp ...)
(detect-coding-string
 (encode-coding-string "こんにちは" 'utf-8)) ; (japanese-shift-jis utf-8 )
(find-coding-systems-string "こんにちは")    ; (japanese-shift-jis japanese-iso-8bit ...)
(find-charset-string "abcほげ")              ; (ascii japanese-jisx0208)
(find-charset-string "abc")                  ; (ascii)

;; 文字コード判別の優先順位を上げるらしい
(prefer-coding-system CODING-SYSTEM)
(prefer-coding-system 'utf-8-unix)
coding-category-list

(encode-coding-string "こんにちは" 'cp932)
(encode-coding-string "こんにちは" 'binary)
(encode-coding-string "こんにちは" 'sjis)
(encode-coding-string "こんにちは" 'euc-jp)
(encode-coding-string "こんにちは" 'utf-8)
(encode-coding-string "こんにちは" 'utf-16)
(decode-coding-string (encode-coding-string "こんにちは" 'utf-8) 'utf-8)

;; Emacsの内部文字列に変換する (UTF-8 らしい)
(string= (string-as-unibyte "こんにちは")
         (encode-coding-string "こんにちは" 'utf-8))

(string-as-multibyte (string-as-unibyte "こんにちは")) ; "こんにちは"

(unibyte-char-to-multibyte (multibyte-char-to-unibyte ?あ)) ; #:ERR
(apply #'make-char (split-char ?あ))    ; 53794 (#o151042, #xd222, ?あ)

(decode-sjis-char (encode-sjis-char ?z))  ; 122 (#o172, #x7a, ?z)
(decode-sjis-char (encode-sjis-char ?こ)) ; 53811 (#o151063, #xd233, ?こ)

;; 同じはずなのだが...この仕様のおかげでデコードできない文字列がある
(string-equal (string ?\201) "\201")           ; nil

(decode-coding-string (string ?\201 ?y) 'sjis) ; "y"
(decode-coding-string "\201y" 'sjis)           ; "【"

(decode-coding-string "\201y('A`)\202\273\202\3143\201z\202P\202Q\214\216\202R\202Q\223\372.mp3" 'sjis)
;;=> "【('A`)その3】１２月３２日.mp3"

(coding-system-list 'base-only)
charset-list
last-coding-system-used

(detect-coding-with-language-environment (point-min)
                                         (min (point-max) #x1000)
                                         "Japanese")
;; (iso-latin-1-unix emacs-mule-unix raw-text-unix no-conversion)
(detect-coding-region (point-min) (min (point-max) #x1000))
;; (iso-latin-1-unix emacs-mule-unix raw-text-unix no-conversion)

(set-language-environment "Japanese")
current-language-environment
(set-default-coding-systems 'japanese-shift-jis-dos)
(set-clipboard-coding-system 'japanese-shift-jis-dos)
(set-w32-system-coding-system 'japanese-shift-jis-dos)
(setq default-file-name-coding-system 'japanese-shift-jis)
(setq-default buffer-file-coding-system 'japanese-shift-jis-dos)
(setq default-terminal-coding-system 'japanese-shift-jis-dos)
(setq default-keyboard-coding-system 'japanese-shift-jis)
(setq default-process-coding-system '(japanese-iso-8bit . japanese-iso-8bit))

(coding-system-doc-string 'junet)  ; "ISO 2022 based 7bit encoding for Japanese (MIME:ISO-2022-JP)."
(coding-system-doc-string 'sjis)   ; "Shift-JIS 8-bit encoding for Japanese (MIME:SHIFT_JIS)."
(coding-system-doc-string 'euc-jp) ; "ISO 2022 based EUC encoding for Japanese (MIME:EUC-JP)."

(coding-system-doc-string 'utf-8)      ; "UTF-8 (no signature (BOM))"
(coding-system-doc-string 'emacs-mule) ; "Emacs 21 internal format used in buffer and string."

;; MIME用charset
(defun mime-charset (coding-system)
  (coding-system-get coding-system :mime-charset))

(mime-charset 'japanese-shift-jis-dos)  ;=> shift_jis
(mime-charset 'japanese-iso-8bit)       ;=> euc-jp
(mime-charset 'mule-utf-8)              ;=> utf-8

;; なぜか utf-8 になっていた
;; (set-language-environment "japanese") のせいかな?
(when (eq system-type 'windows-nt)
  (setq default-process-coding-system
        ;; '(japanese-iso-8bit . japanese-iso-8bit)
        '(sjis-unix . sjis-unix)
        ))

(defun kanji-coding-system-p ()
  (memq (coding-system-base buffer-file-coding-system)
        (get-language-info "Japanese" 'coding-system)))

(defun what-charset-region (from to)
  (interactive "r")
  (message "%s" (find-charset-region from to)))

;; [2016-01-11]
;; BOM あり UTF-8 な html ファイルの中身に <meta charset="utf-8"> が
;; 含まれているとファイル保存時に BOM が取り除かれてしまう

;; 文字コード判定
;; -> (apropos "auto-coding")

;; バッファ内のマルチバイト表示を切り替える (デフォルトは t)
(set-buffer-multibyte enable-multibyte-characters)
(set-buffer-multibyte nil)

(set-charset-priority 'ascii 'japanese-jisx0208 'latin-jisx0201
                      'katakana-jisx0201 'iso-8859-1 'cp1252 'unicode)
(set-coding-system-priority 'utf-8 'euc-jp 'iso-2022-jp 'cp932)

(set-charset-priority 'unicode)

;; ファイルの文字コードを判定する手段はいくつかある
;; 1. `insert-file-contents' + `buffer-file-coding-system'
;;    ファイルの挿入時に Emacs 内部で文字コード判定が行われるのを利用する
;; 2. `insert-file-contents-literally' + `detect-coding-region'
;;    ファイルをバイト列で流し込んで、後から判定を行う
;;
;; あとはファイル挿入時にすべて読み込むか、あるいはメモリ効率を優先して先頭Nバイトだけ読み込むかが悩みどころ
(cl-defun detect-file-coding-system (file &optional (highest t))
  (with-temp-buffer
    (insert-file-contents-literally file nil #x0 #x800)
    (detect-coding-region (point-min) (point-max) highest)))

(detect-file-coding-system (expand-file-name "HELLO" data-directory))
;;=> iso-2022-7bit-unix

;; バイト数を考慮した関数 (`XXX-bytes')
(cons #1=(point-max) (byte-to-position (position-bytes #1#)))
(cons (length #1="こんにちは WORLD 🍣") (string-bytes #1#))

(let ((emoji-font
       (font-spec :family "Segoe UI Emoji" :registry "iso10646-1")))
  (dolist (target
           '((#x1F300 . #x1F5FF)
             (#x1F600 . #x1F64F) ;; Emoticons
             (#x1F650 . #x1F67F) ;; Ornamental Dingbats
             (#x1F680 . #x1F6FF) ;; Transport and Map Symbols
             (#x1F700 . #x1F77F) ;; Alchemical Symbols
             (#x1F780 . #x1F7FF) ;; Geometric Shapes Extended
             (#x1F800 . #x1F8FF) ;; Supplemental Arrows-C
             (#x1F900 . #x1F9FF)
             ))
    (set-fontset-font t target emoji-font nil 'prepend)))

;; ## Unicode 用語集
;; - Unicode正規化
;; - 結合文字列

"\u30DD"                                ;=> "ポ" (length=1)
"\u30db\u309a"                          ;=> "ポ" (length=2)

"\u01d6"                                ;=> "ǖ"
"\u00fc\x0304"                          ;=> "ǖ"
"\u0075\u0308\u0304"                    ;=> "ǖ"

;; TODO:
;; - 結合文字列を1文字として数える方法は？

;; CP50220 (Microsoft iso-2022-jp for mail)
(require 'cp5022x)

(progn
  ;; Emacs は null バイトが含まれているバッファ、リージョン、文字列を確定でバイナリとして扱う
  (let ((inhibit-null-byte-detection nil))
    #0=(detect-coding-string "ASDF\0しかのこのこのここしたんたん")) ;=> (no-conversion)
  ;; non-nil の場合、null バイトを無視して文字コード判別を行う
  (let ((inhibit-null-byte-detection t))
    #0#))			    ;=> (utf-8 utf-8-auto iso-2022-jp)
