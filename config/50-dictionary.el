;; config/translate.el

(use-package google-translate
  :defer t
  :bind (("M-g t" . google-translate-at-point)
         ("M-g T" . google-translate-query-translate))
  :init
  (custom-set-variables
   '(google-translate-default-source-language "en")
   '(google-translate-default-target-language "ja"))
  :config
  (when (boundp 'popwin:special-display-config)
    (add-to-list 'popwin:special-display-config '("*Google Translate*")))
  :ensure t)
