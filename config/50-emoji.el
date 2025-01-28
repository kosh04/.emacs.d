;;; config/emoji

;; Symbola font highly recommended
;; http://users.teilar.gr/~g1951d/

;; 絵文字用のシステムフォントを適用する
(use-package emoji-fontset
  :disabled
  :if (cl-case window-system
        ;; Emacs 25.1 からデフォルトでいい感じの設定が利用可能になった
        (ns (version< emacs-version "25.1"))
        (w32 t))
  :config (emoji-fontset-enable))

;; 豆腐な絵文字を画像で代用する
(use-package emojify
  ;;:hook (emacs-startup . global-emojify-mode)
  :bind ("C-x 8 e a" . emojify-apropos-emoji)
  :custom
  (emojify-display-style 'image) ; ascii/unicode/image
  )

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
  (find-file "https://unicode.org/emoji/charts/emoji-style.txt"))
