;;; config/Basic

(setq kill-whole-line t)

;; 行の切り捨て (non-nil ならば行の折り返し無効)
(setf (default-value 'truncate-lines) t)

;; 入力補完で大文字小文字の区別をしない
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)

;; 補完候補がN以下ならば循環補完
(setq completion-cycle-threshold 3)

(add-to-list 'completion-ignored-extensions ".exe")

;; URL クリックでブラウザを開く
(add-hook 'text-mode-hook 'goto-address-mode)
(add-hook 'prog-mode-hook 'goto-address-prog-mode)

;; バッファ上で URL を開けるようになる
(url-handler-mode +1)

;; `what-cursor-position' [C-u C-x =]
(custom-set-variables
 '(describe-char-unicodedata-file
   (let ((url "http://www.unicode.org/Public/UNIDATA/UnicodeData.txt")
         (path (locate-user-emacs-file "etc/UnicodeData.txt")))
     (unless (file-exists-p path)
       (let ((data (with-temp-buffer
                     (url-insert-file-contents url)
                     (buffer-string))))
         (write-region data nil path nil 'silent)))
     path)))

(setq use-dialog-box nil)
(setq print-quoted t)

;; C-u C-SPC C-SPC ...
(setq set-mark-command-repeat-pop t)

;; ドキュメントの引用符に半角を使用する
;; 全角引用符はターミナル時にテキスト表示がズレる可能性がある
(setq text-quoting-style 'grave)

;; *Help* ウィンドウ表示時にフォーカスする (q 押下で quit)
(setq help-window-select 't)
