;;; font-menu.el --- Simple Font Menu Utility    -*- lexical-binding: t -*-

;; Version: 0.1-git
;; Package-Requires: ((emacs "25.2"))
;; Keywords: font, tools
;; Created: 2019/05/09

;;; Code:

(require 'tabulated-list)


;;; Font Menu

(defvar font-menu-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "w") #'font-menu/copy-font)
    map))

(defun font-menu/copy-font (name)
  "Copy selected font NAME as XLFD format."
  (interactive (list (tabulated-list-get-id)))
  (kill-new name)
  (message "Font:%s" name))

(defun font-menu/-tabulated-desc (font)
  "Return tabulated-list formated FONT description [DESC1 ... DESCN]."
  ;; FIXME: How parse font name with hyphen. e.g. "all-the-icon"
  (or
   ;; `x-decompose-font-name'
   (if (string-match xlfd-tight-regexp font)
       (let ((xlfd-fields (make-vector 12 nil)))
	 (dotimes (i 12)
	   (aset xlfd-fields i (match-string (1+ i) font)))
	 xlfd-fields))
   (apply #'vector (cddr (split-string font "-")))))

(define-derived-mode font-menu-mode tabulated-list-mode
  "Font lists"
  "Major mode for listing the available fonts."
  (setq tabulated-list-format
        [
         ;;("*" 1)
         ;;("maker" 8)
         ("family" 40 t)
         ("weight" 12)
         ("slant" 8)
         ("swidth" 10)
         ("adstyle" 8)
         ("pixelsize" 7)
         ("pointsize" 6)
         ("resx" 5)
         ("resy" 5)
         ("spacing" 7)
         ("avgwidth" 5)
         ("registry" 12)
         ;;("encoding" 8)
         ])
  (setq tabulated-list-entries
        (mapcar (lambda (font)
                  (list font (font-menu/-tabulated-desc font)))
                (x-list-fonts "*")))
  nil)

;;;###autoload
(defun list-fonts ()
  "Display fonts as a table."
  (interactive)
  (with-current-buffer (get-buffer-create "*Fonts*")
    (font-menu-mode)
    (tabulated-list-init-header)
    (tabulated-list-print)
    (display-buffer (current-buffer))))


;;;; Font Family Menu

(defvar font-family-menu/sample-text
  "The quick brown fox jumps over the lazy dog

123456789012345678901234567890
ABCDEFGHIJKLMNOPQRSTUVWXYZabcd
あいうえおかきくけこさしすせそ
◎○●▲■◎○●▲■◎○●▲■
×÷±＋−×÷±＋−×÷±＋−
123456789012345678901234567890
ΑΒΓαβγΑΒΓαβγΑΒΓαβγΑΒΓαβγΑΒΓαβγ

abcdefghijklmnopqrstuvwxyz
ABCDEFGHIJKLMNOPQRSTUVWXYZ
0123456789 A^ []{}()
{[(123)]} ;, :-) ;-)
000000000000000000000000
012345678901234567890123456789
012345678901234567890123456789
012345678901234567890123456789
123,123 125.46,15;,'458 \"\",
123'123 ?%#c@!$&/=-+
oO0|1iljJ

あかさたなはまやらわ
アカサタナハマヤラワ
零壱弐参肆伍陸漆捌玖拾

console.log('oO08 iIlL1 g9qCGQ ~-+=>');
´`''\"\"1lI|¦!Ø0Oo{[()]}.,:
")

(defvar font-family-menu-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'font-family-menu/display-sample-text)
    map))

(define-derived-mode font-family-menu-mode tabulated-list-mode
  "Font Family Menu"
  "Major mode for listing the available font families."
  (let ((ff (font-family-list)))
    (setq ff (delete-dups ff))
    (setq ff (sort ff #'(lambda (x y) (string< (upcase x) (upcase y)))))
    (setq tabulated-list-format `[("Name" 50 t)])
    (setq tabulated-list-entries
          (mapcar (lambda (family)
                    (list family (vector family)))
                  ff))))

(defun font-family-menu/display-sample-text (font-family)
  (interactive
   (list (or (tabulated-list-get-id)
             (completing-read "Font Family: " (font-family-list)))))
  (let ((buffer (get-buffer-create "*Sample Text*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Font Family: " font-family "\n\n")
        (insert (propertize font-family-menu/sample-text 'face `(:family ,font-family))))
      (goto-char (point-min))
      (view-mode t))
    (display-buffer buffer)))

;;;###autoload
(defun list-font-family ()
  "Display listing font family."
  (interactive)
  (let ((buffer (get-buffer-create "*Font Family List*")))
    (with-current-buffer buffer
      (font-family-menu-mode)
      (tabulated-list-init-header)
      (tabulated-list-print))
    (display-buffer buffer)))

(provide 'font-menu)
