;; config/translate.el

(when nil
(require 'google-translate)

(global-set-key (kbd "M-g t") 'google-translate-at-point)
(global-set-key (kbd "M-g T") 'google-translate-query-translate)

(custom-set-variables
 '(google-translate-default-source-language "ja")
 '(google-translate-default-target-language "en"))

(when (boundp 'popwin:special-display-config)
  (add-to-list 'popwin:special-display-config '("*Google Translate*")))
)
