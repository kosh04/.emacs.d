;;; config/company.el

;; テキスト補完
(use-package company
  :pin #:gnu
  :diminish company-mode
  :hook
  ;; TODO: 補完が重くなってきたらglobalをやめて個別にcompany-modeを指定する
  (emacs-startup . global-company-mode)
  :custom
  (company-idle-delay 0.2)
  (company-selection-wrap-around t)
  ;; 補完時はなるべく元のキー入力を優先したい
  :bind (:map company-active-map
              ("RET"      . nil)
              ("<return>" . nil) 
              ("TAB"   . company-complete-selection)
              ("<tab>" . company-complete-selection)
              ("C-h" . delete-backward-char)))

