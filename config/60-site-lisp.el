;;; config/site-lisp

;; TODO: [2019-03-07]
;; MELPA に登録した (あるいは site-lisp 以下にある) 自作パッケージは、可能な限り手動インストールする
;; autoload cookie によるシンボルの事前定義＆遅延ロードが有効になるため、通常パッケージに近い設定を行える

;; -L ~/.emacs.d/site-lisp
(add-to-list 'load-path (locate-user-emacs-file "site-lisp"))

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
  :load-path "~/Documents/GitHub/emacs-wandbox"
  :init
  ;; TODO: bind-key で名前付きプレフィックスキーを定義する方法
  (defalias 'wandbox-command-map
    (let ((map (make-sparse-keymap "Wandbox")))
      (define-key map "w" '("run" . wandbox))
      (define-key map "e" '("eval" . wandbox-eval-last-sexp))
      (define-key map "i" '("insert" . wandbox-insert-template))
      (define-key map "l" '("list" . wandbox-list-compilers))
      map))
  (global-set-key (kbd "C-c w") 'wandbox-command-map)
  :config
  (setq wandbox--verbose t)
  ;; 通信環境の問題でHTTPSがたまに失敗することがある
  '(ignore-errors
     (wandbox-add-server "fetus" "https://wandbox.fetus.jp"))
  )

(use-package m3u-mode
  :mode "\\.m3u8?\\'"
  :load-path "site-lisp/m3u-mode"
  :bind (:map m3u-mode-map ("C-c i" . m3u-insert-entry))
  :config
  (add-hook 'm3u-mode-hook
            (lambda ()
              (highlight-phrase "[+]GENRE")))
  )

(use-package unicode-escape
  :after names
  ;;:load-path "~/Documents/GitHub/unicode-escape.el"
  ;;:demand
  )

(use-package gitignore
  :load-path "site-lisp/gitignore"
  :after gitignore-mode
  :custom
  (gitignore-template-directory
   (file-name-as-directory
    (locate-user-emacs-file "share/autoinsert/gitignore")))
  :bind (:map gitignore-mode-map
              ([remap insert-file] . gitignore-insert-template))
  )

(use-package lingr-irc
  :load-path "site-lisp"
  :commands (lingr-irc))

;; セッション管理
(use-package workgroups2
  :disabled
  :config (workgroups-mode +1))
