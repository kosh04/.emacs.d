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
;;(require 'xyzzy-keymap)
(require 'font-menu)
(require 'user-utils)

(require 'google-search)
(global-set-key (kbd "C-c g") 'google-search)

(use-package wandbox
  ;; 開発版
  :load-path "../site-lisp/wandbox"
  :bind (("C-c w w" . wandbox)
         ("C-c w e" . wandbox-eval-last-sexp)
         ("C-c w i" . wandbox-insert-template)
         ("C-c w l" . wandbox-list-compilers))
  :config
  (setq wandbox--verbose t)
  ;; 通信環境の問題でHTTPSがたまに失敗することがある
  '(ignore-errors
    (wandbox-add-server "fetus" "https://wandbox.fetus.jp"))
  )

(use-package m3u-mode
  :pin #:manual
  :load-path "../site-lisp"
  :bind (:map m3u-mode-map ("C-c i" . m3u-insert-entry))
  )

(use-package unicode-escape
  :pin #:manual
  :load-path "../site-lisp/unicode-escape"
  )

(use-package gitignore
  :load-path "../site-lisp/gitignore"
  :after gitignore-mode
  :pin #:manual
  :custom
  (gitignore-template-directory
   (file-name-as-directory
    (locate-user-emacs-file "share/autoinsert/gitignore")))
  :bind (:map gitignore-mode-map
              ([remap insert-file] . gitignore-insert-template))
  )

(use-package lingr-irc
  :commands (lingr-irc)
  :pin #:manual
  :ensure nil)
