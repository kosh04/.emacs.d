;;; memo/whitespace.el

;; 空白を強調表示する方法いろいろ

(require 'whitespace)
(global-whitespace-mode +1)
;; 全角スペース
(setq whitespace-space-regexp "\\(\x3000+\\)")
(setq whitespace-style '(trailing empty spaces tab-mark face))
(setq whitespace-action '(auto-cleanup))
;; 強調色をおとなしめにする
(set-face-background 'whitespace-trailing "LemonChiffon4")
(set-face-background 'whitespace-empty "LemonChiffon4")
(set-face-background 'whitespace-space "LemonChiffon4")

;;; 全角空白とか改行とかタブを強調表示
;;; おまけ for GNU Emacs - http://homepage3.nifty.com/satomii/software/elisp.ja.html
(require 'jaspace)
(autoload 'jaspace-mode-on "jaspace" nil t)
(setq jaspace-mode '(c-mode))
(setq jaspace-alternate-jaspace-string "□")
(if window-system
    (setq jaspace-alternate-eol-string "\xab\n"))
(setq jaspace-highlight-tabs ?^)	; use ^ as a tab marker

;;; 全角空白、タブなどの強調表示
;;; これだとfont-lockが呼び出されるたびにキーワードが追加されて遅くならないか？
;;; http://eigyr.dip.jp/diary/200712.html#%E5%85%A8%E8%A7%92%E3%82%B9%E3%83%9A%E3%83%BC%E3%82%B9%E3%82%84%E3%82%BF%E3%83%96%E3%82%92%E5%BC%B7%E8%AA%BF

;; http://www.emacswiki.org/emacs/BlankMode -> whitespace.el
;; http://www.emacswiki.org/elisp/show-wspace.el

;; 行末の空白文字を表示
(setq-default show-trailing-whitespace t)
(set-face-background 'trailing-whitespace "plum")
(set-face-background 'trailing-whitespace "SteelBlue") 
;;(set-face-underline 'trailing-whitespace t)

(defun toggle-trailing-whitespace (&optional arg)
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace))
  (font-lock-mode t))

(defun turn-off-show-trailing-whitespace ()
  (interactive)
  (setq show-trailing-whitespace nil))
(add-hook 'shell-mode-hook 'turn-off-show-trailing-whitespace)

;;; タブ、全角スペースを可視化
;; (require 'my-font-lock-mode)

(load "hi-lock-x.el")
;; (global-hi-lock-mode)
(setq hi-lock-file-patterns-policy nil)

;; 補足: delete-trailing-whitespace が標準で利用可能
;; 余分な空白をカット
;; 一応 'query-replace のように確認したほうがいいか？
(defun trim-trailing-whitespace (start end)
  (interactive "*r")
  (save-excursion
    ;; (replace-regexp "[ \t]+$" "" nil start end)
    (perform-replace "[ \t]+$" "" nil t nil nil nil start end)))

(defun trim-whole-tail-whitespace ()
  (interactive "*")
  (trim-trailing-whitespace (point-min) (point-max)))

;; (add-hook 'write-file-hooks 'trim-tail-whitespace)

(add-hook 'before-save-hook 'whitespace-cleanup)

;; 改行を強調表示↵
(require 'whitespace)
(add-to-list 'whitespace-style 'newline-mark)
(setf (cdr (assq 'newline-mark whitespace-display-mappings)) '(?\n [?↵ ?\n] [?$ ?\n]))
