;;; config/emoji

;; 絵文字用のシステムフォントを適用する
(use-package emoji-fontset
  :if (cl-case window-system
        ;; Emacs 25.1 からデフォルトでいい感じの設定が利用可能になった
        (ns (version< emacs-version "25.1"))
        (w32 t))
  :config (emoji-fontset-enable))

;; 豆腐な絵文字を画像で代用する
(use-package emojify
  :disabled t
  :config (add-hook 'after-init-hook #'global-emojify-mode))

(defvar user:emoji-samples
  '(("😀" "GRINNING FACE" ":grinning:")
    ("🙏" "PERSON WITH FOLDED HANDS" ":pray:")
    ("🍣" "SUSHI" ":sushi:")
    ("💩" "PILE OF POO" ":poo:")
    ("🇯🇵" "REGIONAL INDICATOR SYMBOL LETTER JP" ":jp:"))
  "絵文字の表示確認用サンプル (CodePoint Name Emoji)")

(defun list-emoji-display ()
  "[user] Display sample emoji and symbols."
  (interactive)
  (eww "https://github.com/zonuexe/emoji-fontset.el/blob/master/emojis.org"))
