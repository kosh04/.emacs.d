;;; memo/clipboard

;; https://www.emacswiki.org/emacs/Comments_on_CopyAndPaste
;; https://stackoverflow.com/questions/21830813/how-to-kill-yank-code-between-emacs-buffers-using-screen/21833639#21833639
;; https://elpa.gnu.org/packages/xclip.html

;; ターミナルで実行していてもXのクリップボードに切り取り／貼り付けはできないだろうか
(window-system)                         ; nil
(fboundp 'x-get-selection)              ; t
(fboundp 'x-get-selection-internal)     ; nil
(boundp 'x-select-enable-clipboard)     ; nil

interprogram-cut-function
interprogram-paste-function

;; mac+terminal+emacsclient
(setq interprogram-cut-function #'kill-new)
(setq interprogram-cut-function
      (lambda (message)
	(with-temp-buffer
	  (insert message)
	  (let ((deactivate-mark t))
	    (call-process-region (point-min) (point-max) "pbcopy")))))

;; [Edit]->[Copy]
;; キルリングとクリップボードの中途半端な同期
(global-set-key (kbd "ESC w") 'clipboard-kill-ring-save)

;; クリップボードのデータをkillリングに保存する
;; 使い方その１: ブラウザからURLコピー→Emacsで行削除C-k→リンク貼付 けC-y,M-y
(setq save-interprogram-paste-before-kill t)
