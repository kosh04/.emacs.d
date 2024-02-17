;;; config/progmode

;; スペースでインデント
(setq-default indent-tabs-mode nil)

;; コメント
(setq comment-style 'multi-line)

;; 自動改行と欲張りな削除
;; (add-hook 'c-mode-common-hook 'c-toggle-auto-hungry-state)

(require 'electric)

(electric-pair-mode +1)
;;(electric-layout-mode +1)
;;(electric-indent-mode -1)

;;(add-to-list 'electric-pair-pairs '(?< . ?>))
(add-to-list 'electric-pair-pairs '(?{ . ?}))

;;(add-to-list 'electric-layout-rules '(?{ . around))

(with-eval-after-load 'prog-mode
  (define-key prog-mode-map [f5] 'compile))

(defalias 'make #'compile)

;; 関数一覧
(require 'imenu)
(global-set-key (kbd "C-c l") 'imenu)
(add-hook 'emacs-lisp-mode-hook #'imenu-add-menubar-index)

(defun indent-and-next-line (&optional args)
  "インデントして次の行へ."
  (interactive "p")
  (dotimes (x args)
    (indent-according-to-mode)
    (line-move 1)))

(global-set-key (kbd "M-n") 'indent-and-next-line)

;; Language Template
(use-package autoinsert
  :hook (emacs-startup . auto-insert-mode)
  :custom
  (auto-insert-directory (locate-user-emacs-file "share/autoinsert/")))


;; Batch mode (obsolete); use bat-mode
;; https://www.emacswiki.org/emacs/batch-mode.el
(use-package batch-mode
  :disabled
  :mode ("\\.bat\\'" "\\.cmd\\'"))

;; Smart Compile
(use-package smart-compile
  ;;:bind (("C-c c" . smart-compile))
  :config
  (define-key menu-bar-tools-menu [compile]
    '("Compile..." . smart-compile))
  )

;; 関数上部のコメントをまとめて narrow
(setq narrow-to-defun-include-comments t)

(with-eval-after-load 'make-mode
  ;; Visualize ^TAB
  (add-hook 'makefile-mode-hook 'whitespace-mode))

;; M-x which-function-mode
(with-eval-after-load 'which-func
  ;; 黒背景のモードラインに合わせる
  (setf (face-foreground 'which-func) "skyblue"))

;; issue トラッカーをハイライト＆開けるように
;; (add-hook 'prog-mode-hook #'bug-reference-prog-mode)
;; (with-eval-after-load 'bug-reference
;;   ;; debug2(...); とかに反応しないようにしたい
;;   (setq bug-reference-bug-regexp
;;         (concat "\\<" bug-reference-bug-regexp "\\>"))
;;   )

;; TODO 組み込みパッケージ lisp/*.el に記載されているバグリファレンスを参照する
(add-hook 'emacs-lisp-mode-hook 'bug-reference-prog-mode)
(setq-local bug-reference-url-format "https://debbugs.gnu.org/%s")

(use-package xref
  ;; :bind
  ;; (:map xref--xref-buffer-mode-map
  ;;   ("TAB" . xref-next-line)
  ;;   ("DEL" . xref-prev-line))
  ;; FIXME: :hook で設定すると元の値が消える...？
  ;; :hook (xref-after-jump . view-mode)
  :config
  (add-hook 'xref-after-jump-hook #'view-mode)
  )

;; シンボル強調
(use-package symbol-overlay
  :disabled
  :hook (prog-mode . symbol-overlay-mode))

;; M-x minimap-mode
(use-package minimap
  :custom
  (minimap-window-location 'right))

(use-package highlight-indent-guides
  :hook (python-mode yaml-mode)
  ;; FIXME: ダークモードに対応していない？
  :custom
  (highlight-indent-guides-responsive 'stack)
  (highlight-indent-guides-method 'character))
