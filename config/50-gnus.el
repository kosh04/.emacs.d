;;; config/Gnus

;; 使い方が分からない :p

;; Link:
;; - http://gnus.org/manual.html
;; - https://www.emacswiki.org/emacs/Gnus
;; - https://www.emacswiki.org/emacs/GnusTutorial

;; M-x gnus
(use-package gnus
  :custom
  ;;(gnus-init-file "~/.gnus")
  ;;(gnus-select-method '(nntp "news.gmane.org"))
  (gnus-secondary-select-methods
   '((nnml "")
     (nntp "news.gmane.org")
     ;; (nntp "news.eternal-september.org")
     ;; (nntp "nntp.aioe.org")
     ;; (nntp "news.gwene.org")
     ))
  (gnus-interactive-exit nil)         ; `q' quick exit
  ;;(gnus-inhibit-images 'quiet)
  ;;(gnus-use-full-window nil)
  ;;(gnus-use-trees t)
  ;;(gnus-auto-select-next 'quietly)

  :config
  ;; (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
  (add-hook 'gnus-server-mode-hook 'hl-line-mode)
  (add-hook 'gnus-browse-mode-hook 'hl-line-mode)
  (add-hook 'gnus-summary-mode-hook 'hl-line-mode)
  (add-hook 'gnus-group-mode-hook 'hl-line-mode)
  )
