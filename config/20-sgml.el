;;; config/sgml.el

;; xml/xhtml/shml/html5?

(declare-function 'sgml-quote "sgml-mode")
(fset 'sgml-quote-region #'sgml-quote)
(fset 'html-quote-region #'sgml-quote)

(declare-function 'sgml-pretty-print "sgml-mode")

(use-package sgml-mode
  :defer t
  :bind (:map html-mode-map
              ("C-," . sgml-tag)
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

(declare-function 'nxml-backward-up-element "nxml-mode")
(declare-function 'xmltok-start-tag-local-name "xmltok")

(defun nxml-where ()
  "Display the hierarchy of XML elements the point is on as a path.
Original URL `https://www.emacswiki.org/emacs/NxmlMode'"
  (interactive)
  (let ((path nil))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (< (point-min) (point)) ;; Doesn't error if point is at beginning of buffer
                    (condition-case nil
                        (progn
                          (nxml-backward-up-element) ; always returns nil
                          t)
                      (error nil)))
          (setq path (cons (xmltok-start-tag-local-name) path)))
        (if (called-interactively-p t)
            (message "/%s" (mapconcat 'identity path "/"))
          (format "/%s" (mapconcat 'identity path "/")))))))

(defun user:nxml-mode-setup ()
  ;; 編集中もカーソル位置のXPathを表示したい
  (setq-local eldoc-documentation-function #'nxml-where)
  (eldoc-mode))

(with-eval-after-load 'nxml-mode
  (add-hook 'nxml-mode-hook 'user:nxml-mode-setup))

;; Zen Coding
(use-package emmet-mode
  :defer t
  :init (progn
          ;;(custom-set-variables '(emmet-indentation 2))
          (add-hook 'sgml-mode-hook 'emmet-mode)
          (add-hook 'css-mode-hook  'emmet-mode)))

(use-package web-mode
  :pin melpa-stable
  :defer t
  :pin melpa-stable
  :ensure emmet-mode
  ;; :mode (("\\.html?\\'" . web-mode)
  ;;        ("\\.tpl\\.php\\'" . web-mode))
  ;; :config (setq web-mode-script-padding 0
  ;;               web-mode-style-padding 0)
  :config (add-hook 'web-mode-hook 'emmet-mode))


