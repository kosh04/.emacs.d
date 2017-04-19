;;; config/Gnus

;; 使い方が分からない :p

;; Link:
;; - http://gnus.org/manual.html
;; - https://www.emacswiki.org/emacs/GnusTutorial

(custom-set-variables
 '(gnus-select-method '(nntp "news.gmane.org"))
 '(gnus-interactive-exit nil)           ; `q' quick exit
 '(gnus-inhibit-images 'quiet)
 '(gnus-use-full-window nil)
 '(gnus-use-trees t))

(with-eval-after-load 'gnus
  (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
  )
