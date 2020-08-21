;;; config/completion --- 補完機能

;; 入力補完で大文字小文字の区別をしない
;;(setq completion-ignore-case t)
(use-package emacs
  :custom
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  ;; 補完候補が N 以下ならば循環補完
  (completion-cycle-threshold 3)
  )

(add-to-list 'completion-ignored-extensions ".exe")

(use-package company
  :pin #:gnu
  :diminish company-mode
  :hook
  ;; TODO: 補完が重くなってきたらglobalをやめて個別にcompany-modeを指定する
  (emacs-startup . global-company-mode)
  :custom
  (company-idle-delay 0.2)
  (company-selection-wrap-around t)
  (company-show-numbers t)
  ;; 補完時はなるべく元のキー入力を優先したい
  :bind (:map company-active-map
              ("RET"      . nil)
              ("<return>" . nil) 
              ("TAB"   . company-complete-selection)
              ("<tab>" . company-complete-selection)
              ("C-h" . delete-backward-char)))

(use-package company-box
  :disabled
  :hook (company-mode . company-box-mode))
