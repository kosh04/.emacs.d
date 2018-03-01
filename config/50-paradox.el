;;; config/Paradox --- modernizing Emacs Package Menu

(use-package paradox
  :pin "melpa-stable"
  :config
  (custom-set-variables
   '(paradox-execute-asynchronously t)
   ;;'(paradox-homepage-button-string "⛺") ; why cannot use emoji?
   )
  (paradox-enable))
