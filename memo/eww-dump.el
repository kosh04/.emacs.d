#!/usr/bin/env emacs

(require 'eww)

(defun eww-dump-buffer (url)
  ;; w3m -dump みたいに端末にHTMLレンダリング結果を出力したかった
  ;; 実際は生のHTMLを返すだけ...
  (with-temp-buffer
    (url-insert-file-contents url)
    (let* ((charsets (detect-coding-region (point-min) (point-max)))
           (charset (if charsets (car charsets) 'utf8)))
      (save-window-excursion
        (eww-display-html charset url)))
    (buffer-string)))

(let ((url "http://www.1101.com/nintendo/miyamoto2015/2015-12-04.html"))
  (print (eww-dump-buffer url)))
