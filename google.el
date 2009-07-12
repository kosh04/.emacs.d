;;; -*- mode:emacs-lisp; coding:utf-8 -*-
;;; google.el --- 簡易 Google 検索

;; 元ネタ: 自慢の.emacsを貼り付けよう スレ 198
;; http://www.geocities.co.jp/SiliconValley-SanJose/7225/log/1001393679.html#R198
(defun google-encoding (str)
  ;; (setq str (encode-coding-string str 'shift_jis))
  (setq str (encode-coding-string str 'utf-8))
  (let* ((len (length str))
         (ret (make-string (* len 3) ?a))
         (i 0) (j 0) char type)
    (while (< i len)
      (setq char (aref str i))
      (if (< char 126)
          (aset ret j char)
        (aset ret j ?%)
        (setq j (1+ j))
        (aset ret j (aref "0123456789ABCDEF" (lsh char -4)))
        (setq j (1+ j))
        (aset ret j (aref "0123456789ABCDEF" (logand char 15))))
      (setq i (1+ i) j (1+ j)))
    (substring ret 0 j)))

;; "&hl=ja&lr=lang_ja&num=100"
(defvar google-search-url-options nil)

;;;###autoload
(defun google-search (string)
  (interactive (list (read-string "Google: "
                                  (if mark-active
                                      (buffer-substring (region-beginning)
                                                        (region-end))
                                      (current-word)))))
  ;; (require 'browse-url)
  (browse-url (concat "http://www.google.com/search?q="
                      ;; (google-encoding str)
                      (mapconcat #'url-hexify-string (split-string string) "+")
                      (or google-search-url-options "")
                      )))
(provide 'google)
