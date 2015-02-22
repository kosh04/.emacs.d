;;; google.el --- 簡易 Google 検索		-*- coding: utf-8 -*-

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

(if (fboundp 'url-hexify-string)
    (fset 'google-encoding #'url-hexify-string))

;; ブラウザをw3mに変更する場合は
;; (require 'w3m)
;; (setq google-search-browser-function 'w3m-browse-url)
(defvar google-search-browser-function browse-url-browser-function)

;; "&hl=ja&lr=lang_ja&num=100"
(defvar google-search-url-options
  "&ie=UTF-8&oe=UTF-8")

;;;###autoload
(defun google-search (string)
  (interactive
   (list (read-string "Google: "
                      (cond (mark-active
                             (buffer-substring-no-properties
                              (region-beginning)
                              (region-end)))
                            (:else
                             (current-word))))))
  ;; (require 'browse-url)
  (let ((browse-url-browser-function google-search-browser-function))
    (browse-url (concat "http://www.google.com/search?q="
                        (mapconcat #'google-encoding
                                   (split-string string)
                                   "+")
                        (or google-search-url-options "")
                        ))))

;; (global-set-key (kbd "C-c g") 'google-search)

(provide 'google)

;;; google.el ends here.
