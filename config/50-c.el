;;; config/C,C++

(dolist (ext '(".gcov" ".gcda" ".gcno" ".gcda"))
  (add-to-list 'completion-ignored-extensions ext))

(use-package cquery
  :disabled
  :custom
  (cquery-executable "~/opt/bin/cquery")
  ;;(cquery-extra-init-params '(:completion (:detailedLabel t)))
  :hook
  (c-mode . lsp-cquery-enable))

(use-package c-eldoc
  :disabled
  :init (add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
  :config
  (custom-set-variables
   '(c-eldoc-cpp-command "cpp")
   '(c-eldoc-includes "-I. -I..")))

(use-package company-c-headers
  :after (company)
  :config
  (add-to-list 'company-backends 'company-c-headers))

(use-package irony
  :disabled
  :init
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'c++-mode-hook 'irony-mode)
  :custom
  (irony-user-dir "~/.emacs.d/share/irony/")
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
  :init (add-to-list 'irony-mode-hook 'irony-eldoc))

(use-package company-irony
  :after company
  :config (add-to-list 'company-backends 'company-irony))

;; TODO: rtags
;; http://www.rtags.net/
(use-package rtags)

(use-package ccls
  :custom
  (ccls-sem-highlight-method 'font-lock)
  (ccls-args '("-log-file=/tmp/ccls.log" "-v=1"))
  (ccls-initialization-options
   (cond
    ((eq system-type 'darwin)
     ;; https://github.com/MaskRay/ccls/issues/191
     '(:clang
       (:extraArgs
        ["-isystem/usr/local/include"
         ;; "-isystem/Library/Developer/CommandLineTools/usr/include/c++/v1"
         ;; "-isystem/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include"
         ;; "-isystem/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/11.0.0/include"
         ;; "-isystem/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include"
         ;; "-isystem/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include"
         ;; "-isystem/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/System/Library/Frameworks"
         ])))
    (t nil))))
(add-hook 'c-mode-common-hook 'lsp-deferred)
