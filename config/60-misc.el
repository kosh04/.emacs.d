;; config/misc

(require 'user-utils)
(use-package f)
(use-package s)

(defun user:emacs-init-time ()
  (message "Emacs init time %s" (emacs-init-time)))

(add-hook 'emacs-startup-hook #'user:emacs-init-time)

(defadvice locate (around modify-buffer-name activate)
  (let ((locate-buffer-name (format "*Locate %s*" search-string)))
    ad-do-it))

;; http://www.bookshelf.jp/texi/elisp-manual-20-2.5-jp/elisp_38.html#SEC610
(defun insert-time ()
  "日付と時刻の挿入."
  (interactive)
  (insert (iso8601)))

(global-set-key (kbd "C-c t") 'insert-time)

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

(diminish 'auto-revert-mode)

(use-package csv-mode
  ;; netscape cooke text is tsv format
  :mode "/cookie.txt\\'"
  :config
  (add-hook 'csv-mode-hook 'hl-line-mode))

;;  View Large Files
;; https://github.com/m00natic/vlfi
(use-package vlf)

(defun user/execute-extended-command-other-frame ()
  "Execute command (M-x) in other frame."
  (interactive)
  (make-frame)
  (call-interactively 'execute-extended-command))

(global-set-key (kbd "C-x 5 x") 'user/execute-extended-command-other-frame)

;; (use-package tar-mode
;;   :bind (:map tar-mode-map ("f" . tar-view)))

(with-eval-after-load 'tar-mode
  (advice-add 'tar-extract :after
              (lambda (&rest args)
                (when view-read-only
                  (read-only-mode)))))

(use-package pangu-spacing
  :defer t
  :diminish "Pangu"
  :config (global-pangu-spacing-mode +1))

(defun init-loader-display-chart ()
  "init-loader の読み込み時間をグラフ表示します."
  (interactive)
  (require 'chart)
  (require 'map)
  (let (acc)
    (with-current-buffer "*init log*"
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "loaded \\(.*\\)[.] \\([0-9]*\\.?[0-9]+\\|[0-9]+\\)" nil t)
          (push (cons (file-name-base (match-string 1))
                      (* 1000 (string-to-number (match-string 2))))
                acc))))
    (chart-bar-quickie
     'horizontal ;;'vertical
     "Load time of init-loader"
     (map-keys acc) "Name"
     (map-values acc) "Time (ms)"
     10
     (lambda (x y) (> (cdr x) (cdr y))))
    ))
;;(add-hook 'emacs-startup-hook 'init-loader-display-chart)

;; カーソル移動時の見た目がちょっとだけスタイリッシュになる
(use-package beacon
  :disabled
  :init (beacon-mode +1))

(use-package highlight-indentation
  :disabled
  :hook (prog-mode . highlight-indentation-mode)
  :config
  ;;(setf (face-background 'highlight-indentation-face) "#e3e3d3")
  ;;(setf (face-background 'highlight-indentation-current-column-face) "#c3b3b3")
  nil)
