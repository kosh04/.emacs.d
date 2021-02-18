;;; config/Basic

(setq kill-whole-line t)

;; 行の切り捨て (non-nil ならば行の折り返し無効)
(setf (default-value 'truncate-lines) t)

;; URL クリックでブラウザを開く
(add-hook 'text-mode-hook 'goto-address-mode)
(add-hook 'prog-mode-hook 'goto-address-prog-mode)

;; バッファ上で URL を開けるようになる
(url-handler-mode +1)

;; `what-cursor-position' [C-u C-x =]
(let ((url "http://www.unicode.org/Public/UNIDATA/UnicodeData.txt")
      (path (locate-user-emacs-file "share/etc/UnicodeData.txt")))
  (unless (file-exists-p path)
    (let ((data (with-temp-buffer
                  (url-insert-file-contents url)
                  (buffer-string))))
      (write-region data nil path nil 'silent)))
  (custom-set-variables
   `(describe-char-unicodedata-file ,path)))

(setq use-dialog-box nil
      use-file-dialog nil)
(setq print-quoted t)

;; C-u C-SPC C-SPC ...
(setq set-mark-command-repeat-pop t)

;; ドキュメントの引用符に半角を使用する
;; 全角引用符はターミナル時にテキスト表示がズレる可能性がある
(setq text-quoting-style 'grave)

;; *.gnu.org とのリクエスト通信がよろしくない場合 (400 Bad Request) に有効 ?
(customize-set-variable
 'gnutls-algorithm-priority
 (if (version< emacs-version "26.3")
     "NORMAL:-VERS-TLS1.3"))

(global-auto-revert-mode +1)

(define-advice quit-window (:filter-args (args) kill-or-bury)
  "ファイル閲覧時は q キー押下でバッファごと削除する. (デフォルトは bury)
ただし *help* などの特殊バッファ時には影響しない."
  (when (and args (buffer-file-name))
    ;; args=(&optional kill window)
    ;; bury-or-kill を反転させる
    (setf (elt args 0) (not current-prefix-arg)))
  args)

(setq temp-buffer-show-function #'pop-to-buffer)

;; C-x = で文字名情報を追加する
(setq what-cursor-show-names t)
