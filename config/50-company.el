;;; config/company.el

;; テキスト補完
(use-package company
  :config
  ;; TODO: 補完が重くなってきたらglobalをやめて個別にcompany-modeを指定する
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-idle-delay 0.2)
  (setq company-selection-wrap-around t)
  ;; 補完時はなるべく元のキー入力を優先したい
  (unbind-key "<return>" company-active-map)
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection)
              ("C-h" . delete-backward-char)))

