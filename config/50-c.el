;;; config/C

(dolist (ext '(".gcov" ".gcda" ".gcno" ".gcda"))
  (add-to-list 'completion-ignored-extensions ext))

(use-package cquery
  :config
  (add-hook 'c-mode-hook 'lsp-cquery-enable)
  (setq cquery-executable "~/opt/bin/cquery")
  ;;(setq cquery-extra-init-params '(:completion (:detailedLabel t)))
  )

(use-package c-eldoc
  :disabled (featurep 'cquery)
  :defer t
  :init (add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
  :config
  (custom-set-variables
   '(c-eldoc-cpp-command "cpp")
   '(c-eldoc-includes "-I. -I..")))

(use-package company-c-headers
  :defer t
  :after (company)
  :config
  (add-to-list 'company-backends 'company-c-headers))

(use-package irony
  :disabled (featurep 'cquery)
  :defer t
  :init
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'c++-mode-hook 'irony-mode)
  :config
  (if (boundp 'w32-pipe-read-delay)
      (setq w32-pipe-read-delay 0))

  (when (eq system-type 'darwin)
    ;;(setf (getenv "LD_LIBRARY_PATH") "/usr/local/opt/llvm/lib")
    (setf (getenv "LD_LIBRARY_PATH") "/Library/Developer/CommandLineTools/usr/lib")
    )
  )

(use-package irony-eldoc
  :after irony
  :config (add-to-list 'irony-mode-hook 'irony-eldoc))

(use-package company-irony
  :after company
  :config (add-to-list 'company-backends 'company-irony))

;; TODO: rtags
;; http://www.rtags.net/
(use-package rtags)
