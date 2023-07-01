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
