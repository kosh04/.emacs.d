;;; config/company.el

;; テキスト補完
(use-package company
  :pin gnu
  :diminish company-mode
  :init
  ;; TODO: 補完が重くなってきたらglobalをやめて個別にcompany-modeを指定する
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-idle-delay 0.2)
  (setq company-selection-wrap-around t)
  ;; 補完時はなるべく元のキー入力を優先したい
  :bind (:map company-active-map
              ("RET"      . nil)
              ("<return>" . nil) 
              ("TAB"   . company-complete-selection)
              ("<tab>" . company-complete-selection)
              ("C-h" . delete-backward-char))
  :ensure t)

