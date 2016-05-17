;;; memo/coding-systems.el -- about encoding

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
