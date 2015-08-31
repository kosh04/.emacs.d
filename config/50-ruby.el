;;; config/ruby.el

(use-package inf-ruby
  :defer t
  :init (defalias 'irb 'inf-ruby)
  )
