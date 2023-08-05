;; config/Dictionary

(defun user:google-translate-whole-buffer ()
  "バッファ全体をGoogle翻訳します."
  (interactive)
  (save-mark-and-excursion
    (progn ;; or `mark-whole-buffer'
      (goto-char (point-min))
      (push-mark (point-max) nil t))
    (google-translate-at-point)))

(use-package google-translate
  :custom
  (google-translate-default-source-language "auto")
  (google-translate-default-target-language "ja")

  :preface
  (defun user:google-translate-at-point ()
    "カーソル位置の文字を判別してGoogle翻訳します."
    (interactive)
    (let ((asciip (string-match "[[:ascii:]]" (string (following-char)))))
      (if asciip
          (google-translate-at-point)
          (google-translate-at-point-reverse))))

  :bind (("C-c e" . user:google-translate-at-point)
         ("M-g T" . google-translate-query-translate))

  :config
  (with-eval-after-load 'popwin
    (add-to-list 'popwin:special-display-config '("*Google Translate*" :height 0.5 :stick t)))
  )

;; 辞書.app
(use-package osx-dictionary
  :disabled
  :if (eq system-type 'darwin)
  :bind (("C-c e" . osx-dictionary-search-word-at-point))
  )

;; 英英辞書
(use-package dictionary
  :bind (("M-s d" . dictionary-search))
  :custom
  (dictionary-server "dict.org"))
