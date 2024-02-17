;;; config/whitespace

(use-package whitespace
  :custom
  (whitespace-style '(tabs tab-mark face trailing lines-tail newline-mark))
  ;; N+1行目以降をハイライト (lines-tail)
  (whitespace-line-column 100)
  ;; FIXME: 特殊バッファの末尾空白は見逃してほしい (e.g. shell)
  (show-trailing-whitespace nil)
  ;; :hook (prog-mode . whitespace-mode)
  :config
  (setf (cdr (assq 'newline-mark whitespace-display-mappings))
        '(?\n [?\u21B5 ?\n] [?$ ?\n])) ;; ↵
  )

(defun user/show-trailing-whitespace ()
  (setq show-trailing-whitespace t))

;; 全角スペースのハイライトが不要ならば
;; (setq nobreak-char-display nil)
