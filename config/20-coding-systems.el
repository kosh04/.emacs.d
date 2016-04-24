;;; config/coding-systems.el

;; UTF-8 で統一したいが Windows の日本語ファイル名が文字化けする...
;;(set-default-coding-systems 'cp932)
;;(prefer-coding-system 'cp932)
(prefer-coding-system 'utf-8-auto)

;;(setq save-buffer-coding-system 'utf-8-unix)

(defun detect-and-decode-string (string)
  (mapcar #'(lambda (encoding)
              (cons encoding (decode-coding-string string encoding)))
          (detect-coding-string string)))

(fset 'encode #'encode-coding-string)
(fset 'decode #'decode-coding-string)
(fset 'detect #'detect-coding-string)

(with-eval-after-load 'mule
  ;; <meta charset="utf-8">
  (defun user/leave-utf-8-with-signature (coding-system)
    "[bug] UTF-8 BOM ファイルを保存する時に BOM が削られるのを回避する."
    (when coding-system
      (let ((coding-prop (coding-system-plist coding-system))
            (buffer-prop (coding-system-plist buffer-file-coding-system)))
        ;; UTF-16-BOM でも大丈夫なはず
        (if (and (eq (plist-get coding-prop :coding-system)
                     (plist-get buffer-prop :coding-system))
                 (eq (plist-get buffer-prop :bom) t))
            nil                         ; ignore given coding system
            coding-system))))

  (advice-add 'sgml-xml-auto-coding-function       :filter-return #'user/leave-utf-8-with-signature)
  (advice-add 'sgml-html-meta-auto-coding-function :filter-return #'user/leave-utf-8-with-signature)
  ;;(setq auto-coding-functions nil)
  nil)

;; config/encoding.el ends here.
