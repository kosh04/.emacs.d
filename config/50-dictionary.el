;; config/Dictionary

(use-package google-translate
  :defer t
  :bind (("C-c e" . user:google-translate-at-point)
         ("M-g T" . google-translate-query-translate))
  :config
  (custom-set-variables
   '(google-translate-default-source-language "auto")
   '(google-translate-default-target-language "ja"))

  (defun user:google-translate-at-point ()
    (interactive)
    (let ((asciip (string-match "[[:ascii:]]" (string (following-char)))))
      (if asciip
          (google-translate-at-point)
          (google-translate-at-point-reverse))))
  
  (with-eval-after-load 'popwin
    (add-to-list 'popwin:special-display-config '("*Google Translate*" :height 0.5 :stick t)))
  )
