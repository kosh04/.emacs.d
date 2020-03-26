;;; config/PHP

(use-package php-mode
  :config
  (setq php-search-url "http://www.php.net/ja/")        ; C-c C-f
  (setq php-manual-url "http://www.php.net/manual/ja/") ; C-c C-m
  (setq php-mode-force-pear t)
  (defun php-user-hook ()
    (define-abbrev php-mode-abbrev-table "ex" "extends")
    ;; (setq comment-start "// " comment-end "")
    (c-set-offset 'arglist-intro '+) ; array(...) インデントをスマートにする
    (c-set-offset 'arglist-close 0))
  (add-hook 'php-mode-hook #'php-user-hook)
  :bind (:map php-mode-map ("RET" . newline-and-indent)))
