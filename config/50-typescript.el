;;; config/TypeScript

;; TODO: typescript.el / typescript-mode.el の使いやすさの違いは？

(use-package typescript-mode
  :preface
  (defun typescript-user-hook ()
    (eldoc-mode +1)
    ;;(company-mode +1)
    )
  :hook (typescript-mode . typescript-user-hook))

;; 補完/定義ジャンプ
(use-package tide
  :after typescript-mode
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         ;; (before-save . tide-format-before-save)
         ))
