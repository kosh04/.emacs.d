;; config/translate.el

(use-package google-translate
  :defer t
  :bind (("C-c e" . user:google-translate-at-point)
         ("M-g T" . google-translate-query-translate))
  :config
  (defun user:google-translate-at-point ()
    (interactive)
    (let* ((asciip (string-match "[[:ascii:]]" (string (following-char))))
           (google-translate-default-source-language (if asciip "en" "ja"))
           (google-translate-default-target-language (if asciip "ja" "en")))
      (google-translate-at-point)))

  (when (boundp 'popwin:special-display-config)
     (add-to-list 'popwin:special-display-config '("*Google Translate*")))
  :ensure popwin)
