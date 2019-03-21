;;; config/site-lisp

;; TODO: [2019-03-07]
;; MELPA に登録した (あるいは site-lisp 以下にある) 自作パッケージは、可能な限り手動インストールする
;; autoload cookie によるシンボルの事前定義＆遅延ロードが有効になるため、通常パッケージに近い設定を行える
;; e.g. (use-package NAME :pin #:manual)

(require 'textproc)
(require 'gitter-irc)
(require 'cl-compatible)
(require 'ssh-public-key-overlay)
(require 'data-uri)
(require 'xyzzy)
(require 'xyzzy-keymap)
(require 'user-utils)

(require 'google-search)
(global-set-key (kbd "C-c g") 'google-search)

(use-package m3u-mode
  :bind (:map m3u-mode-map ("C-c i" . m3u-insert-entry)))

(use-package unicode-escape
  :pin #:manual)

(use-package gitignore
  :after gitignore-mode
  ;;:load-path "~/Documents/GitHub/gitignore.el"
  :pin #:manual
  :custom
  (gitignore-template-directory
   (file-name-as-directory
    (locate-user-emacs-file "share/autoinsert/gitignore")))
  :config
  (with-eval-after-load 'gitignore-mode
    (define-key gitignore-mode-map [remap insert-file] #'gitignore-insert-template))
  )

(use-package lingr-irc
  :commands (lingr-irc))
