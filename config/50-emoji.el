;;; config/emoji

;; (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend)

;; 絵文字用のシステムフォントを適用する
(use-package emoji-fontset
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
