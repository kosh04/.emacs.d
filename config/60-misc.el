;; config/misc

(use-package f)
(use-package s)

(setq read-answer-short t) ;; y-or-n-p

(defun user::emacs-init-time ()
  (message "Emacs init time %s" (emacs-init-time)))

(add-hook 'emacs-startup-hook #'user::emacs-init-time)

(defadvice locate (around modify-buffer-name activate)
  (let ((locate-buffer-name (format "*Locate %s*" search-string)))
    ad-do-it))

;; http://www.bookshelf.jp/texi/elisp-manual-20-2.5-jp/elisp_38.html#SEC610
(defun insert-time ()
  "日付と時刻の挿入."
  (interactive)
  (insert (iso8601)))

(global-set-key (kbd "C-c i t") 'insert-time)

;; C-w で直前の単語を削除する (Bash 風)
(define-key minibuffer-local-map (kbd "C-w") 'delete-backward-word)

;;; find-file-at-point
;; カーソル上にあるファイル名や URL を開く
;; フツーの Find file: は C-u C-x C-f
;; (require 'ffap)
;; (ffap-bindings)

(defun ffap-emacs ()
  "URLをemacsのバッファに開く."
  (interactive)
  (url-handler-mode t)
  (let ((ffap-url-fetcher #'find-file))
    (call-interactively #'ffap)))

;; Elnode パッチ
(with-eval-after-load 'elnode
  (defun elnode--http-send-bytes (f httpcon text)
    "[user] monkey patch for `elnode-http-send-string' as raw bytes.
see also URL `https://github.com/nicferrier/elnode/pull/101'"
    (funcall f httpcon (encode-coding-string text 'raw-text)))

  (advice-add 'elnode-http-send-string :around 'elnode--http-send-bytes)
  ;;(advice-remove 'elnode-http-send-string 'elnode--http-send-as-bytes)
  nil)

(with-eval-after-load 'help-mode
  (define-key help-mode-map "[" #'help-go-back)
  (define-key help-mode-map "]" #'help-go-forward))

;; OpenVPN 接続設定ファイル
(add-to-list 'auto-mode-alist '("\\.ovpn\\'" . conf-mode))

;; HTTP Archive (HAR)
(add-to-list 'auto-mode-alist '("\\.har\\'" . json-mode))

;; (use-package helm
;;   :pin melpa-stable)

(defun check-parens-local ()
  (when buffer-file-name
    (add-hook 'after-save-hook 'check-parens nil t)))

;;(add-hook 'text-mode-hook 'check-parens-local)

(custom-set-variables
 ;; コンパイルバッファの出力を追う (M-x compile)
 '(compilation-scroll-output 'first-error))

(when (fboundp 'diminish)
  (diminish 'auto-revert-mode))

(use-package csv-mode
  ;; netscape cooke text is tsv format
  :mode "/cookie.txt\\'"
  :config
  (add-hook 'csv-mode-hook 'hl-line-mode))

;;  View Large Files
;; https://github.com/m00natic/vlfi
(use-package vlf)

(use-package pangu-spacing
  :diminish "Pangu"
  :config (global-pangu-spacing-mode +1))

(defun init-loader-display-chart (&optional max-lines)
  "init-loader の読み込み時間をグラフ表示します.
MAX-LINES はグラフデータの表示数を指定します. (5 or more)"
  (interactive "p")
  (require 'chart)
  (setq max-lines (max max-lines 5))
  (let (names times)
    (dolist (msg (symbol-value 'init-loader--log-buffer))
      (when (string-match "loaded \\(.*\\)[.] \\([0-9]*\\.?[0-9]+\\|[0-9]+\\)" msg)
        (push (file-name-base (match-string 1 msg)) names)
        (push (* 1000 (string-to-number (match-string 2 msg))) times)))
    (chart-bar-quickie
     'horizontal ;;'vertical
     "Load time of init-loader"
     names "Name"
     times "Time (ms)"
     max-lines
     (lambda (x y) (> (cdr x) (cdr y))))
    ))
;;(add-hook 'emacs-startup-hook 'init-loader-display-chart)

;; カーソル移動時の見た目がちょっとだけスタイリッシュになる
(use-package beacon
  :disabled
  :demand
  :config (beacon-mode +1))

(use-package highlight-indentation
  :disabled
  :hook (prog-mode . highlight-indentation-mode)
  :config
  ;;(setf (face-background 'highlight-indentation-face) "#e3e3d3")
  ;;(setf (face-background 'highlight-indentation-current-column-face) "#c3b3b3")
  nil)

;; https://github.com/kosh04/.emacs.d/issues/1
(with-eval-after-load 'compile
  (require 'ansi-color)
  (defun colorize-compilation-buffer ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region
       compilation-filter-start (point))))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
  (add-hook 'compilation-filter #'ansi-color-compilation-filter)
  )

;; 行が長ーーーいファイルの読み込みによるパフォーマンス低下を抑える (min.js,json,etc)
;; https://www.emacswiki.org/emacs/SoLong
;; (package-install-file "https://git.savannah.nongnu.org/cgit/so-long.git/plain/so-long.el")
(use-package so-long
  ;;:load-path "site-lisp/_vendor"
  :demand
  :config
  (global-so-long-mode)
  (add-to-list 'so-long-target-modes 'json-mode))

;; FIXME:
;;
;; json-mode が javascript-mode (js-mode のエイリアス) を継承しているため
;; (apply #'derived-mode-p so-long-target-modes) が nil を返すのはバグ？
;;
;; (apply #'provided-mode-derived-p 'json-mode so-long-target-modes) ;=> nil
;; (apply #'provided-mode-derived-p 'js-mode so-long-target-modes)   ;=> prog-mode

;; モードラインのマイナーモードを纏める ;-)
;; no more `:diminish' ?
(use-package minions
  :demand
  :custom
  ;;(minions-mode-line-lighter "...") ; "[+]"
  (minions-direct
   '(lsp-mode
     flycheck-mode))
  :config
  (minions-mode +1))

(use-package calendar
  :bind (("C-c t c" . calendar)
         :map calendar-mode-map
         ("f" . calendar-forward-day)
         ("b" . calendar-backward-day)
         ("n" . calendar-forward-week)
         ("p" . calendar-backward-week))
  :custom
  (calendar-mark-holidays-flag t)
  (calendar-date-style 'iso)
  (calendar-latitude 37.916192)
  (calendar-longitude 139.036413)
  :config
  (add-hook 'calendar-today-visible-hook 'calendar-mark-today))

;; 国民の祝日
(use-package japanese-holidays
  :after calendar
  :demand
  :config
  (setq calendar-holidays
        (append japanese-holidays
                holiday-local-holidays
                holiday-other-holidays))
  (add-hook 'calendar-today-visible-hook 'japanese-holiday-mark-weekend)
  (add-hook 'calendar-today-invisible-hook 'japanese-holiday-mark-weekend))

(use-package keyfreq
  :demand
  :bind ([f1 f2] . keyfreq-show)
  :config
  (define-advice keyfreq-show (:after (&rest _) read-only)
    (with-current-buffer keyfreq-buffer
      (view-mode)))
  (keyfreq-mode +1))

(use-package chart
  :commands
  (chart-test-it-all
   chart-file-count
   chart-space-usage
   chart-emacs-storage
   chart-emacs-lists
   chart-rmail-from))

;; cache directory
(custom-set-variables
 '(auto-save-list-file-prefix (concat user-emacs-directory "cache/auto-save-list/.saves-"))
 '(url-configuration-directory (locate-user-emacs-file "cache/url"))
 '(image-dired-dir (locate-user-emacs-file "cache/image-dired")))

(use-package man
  :bind
  (:map Man-mode-map
	("j" . scroll-up-line)
	("k" . scroll-down-line)))

(customize-set-value
 'next-error-found-function
 #'(lambda (_from _to) (view-mode +1)))

(use-package remember
  :custom
  (remember-notes-initial-major-mode 'markdown-mode)
  ;; hijack the *scratch* buffer
  ;(remember-notes-buffer-name "*scratch*")
  ;(initial-buffer-choice 'remember-notes)
  :bind
  (("C-c m" . remember)))

(with-eval-after-load 'image
  ;; for open webp image
  (setq image-use-external-converter t))

(use-package gcmh
  :demand
  :config (gcmh-mode +1))

;; ゴミ箱用ファイラ
(use-package trashed
  :if (memq system-type '(gnu/linux windows-nt)))

(use-package gnutls
  :custom
  ;; disable TLSv1.3
  (gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(use-package* mouse
  :when (version<= "28.1" emacs-version)
  :config
  ;; 右クリックメニュー
  (context-menu-mode +1))

(use-package avy
  :bind
  (:map goto-map
        ("c" . avy-goto-char)
        ("l" . avy-goto-line)
        ("w" . avy-goto-subword-1))
  ;;:custom
  ;;(avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  )

;; TODO: ?append to 20-keymaps.el
(use-package swap-buffers
  :bind ("C-c t b" . swap-buffers))
