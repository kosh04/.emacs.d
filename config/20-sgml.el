;;; config/sgml.el

;; xml/xhtml/shml/html5?

(autoload 'sgml-quote "sgml-mode" nil t)
(fset 'sgml-quote-region #'sgml-quote)
(fset 'html-quote-region #'sgml-quote)

(use-package sgml-mode
  :defer t
  :bind (("C-," . sgml-tag)
         ("C-." . sgml-close-tag)))

(defun unhtml-region (start end)
  "HTMLタグを除去する."
  (interactive "*r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (while (re-search-forward "<[^>]*>" nil t)
        (replace-match "")))))

;;(define-key html-mode-map (kbd "C-c C-q") 'sgml-quote)

;; Zen Coding
(use-package emmet-mode
  :defer t
  :init (progn
          ;;(custom-set-variables '(emmet-indentation 2))
          (add-hook 'sgml-mode-hook 'emmet-mode)
          (add-hook 'css-mode-hook  'emmet-mode)))

(use-package web-mode
  :defer t
  :ensure emmet-mode
  ;; :mode (("\\.html?\\'" . web-mode)
  ;;        ("\\.tpl\\.php\\'" . web-mode))
  ;; :config (setq web-mode-script-padding 0
  ;;               web-mode-style-padding 0)
  :config (add-hook 'web-mode-hook 'emmet-mode))

