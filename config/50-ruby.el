;;; config/ruby.el

(use-package inf-ruby
  :defer t
  :init (defalias 'irb 'inf-ruby)
  :config
  (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)
  (add-hook 'compilation-filter-hook 'inf-ruby-auto-enter)
  :bind (:map inf-ruby-minor-mode-map
              ;;("C-c C-s" inf-ruby-console-auto)
              ))
