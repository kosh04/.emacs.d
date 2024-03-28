;;; config/completion --- 補完機能

;; TODO: rename to "autocomplete" ?

;; - icomplete (fido-mode)
;; - company

;; 入力補完で大文字小文字の区別をしない
;;(setq completion-ignore-case t)
(use-package emacs
  :custom
  (tab-always-indent 'complete)
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  ;; 補完候補が N 以下ならば循環補完
  (completion-cycle-threshold 3)
  ;; シンボル補完時に注釈を追加 (@28.1)
  (completions-detailed t)
  :config
  (add-to-list 'completion-ignored-extensions ".exe")
  )

;; XXX: reading-init.el~20240323@ashton314-emacs-bedrock
(use-package emacs
  :disabled
  :config
  (setopt completion-auto-help 'always)
  (setopt completions-max-height 20)
  (setopt completions-format 'one-column)
  (setopt completions-group t)
  (setopt completion-auto-select 'second-tab)
  ;;(setopt completion-auto-select t)
  )

(use-package emacs
  :if (version<= "28.1" emacs-version)
  ;; :hook (after-init . fido-vertical-mode)
  ;; :hook (after-init . icomplete-mode)
  :hook (after-init . icomplete-vertical-mode)

  :hook (icomplete-minibuffer-setup . my-icomplete-styles)
  :preface
  (defun my-icomplete-styles ()
    ;;(setq-local completion-styles '(initials flex))
    (setq-local completion-styles '(basic partial-completion emacs22)))

  :custom
  (read-extended-command-predicate #'command-completion-default-include-p)
  )

(use-package company
  :pin #:gnu
  :diminish company-mode
  ;;:hook
  ;; TODO: 補完が重くなってきたらglobalをやめて個別にcompany-modeを指定する
  ;; (emacs-startup . global-company-mode)
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

(use-package company-statistics
  :hook (after-init . company-statistics-mode)
  :custom
  (company-statistics-file
   (locate-user-emacs-file "cache/company-statistics-cache.el"))
  (company-statistics-auto-restore nil))

(use-package company-emoji
  :after company
  :config (company-emoji-init))

;; 注釈付き補完
;; 注釈一覧 -> `marginalia-annotator-registry'
(use-package marginalia
  :unless (bound-and-true-p completions-detailed)
  :hook (after-init . marginalia-mode))

(use-package prescient
  :disabled
  :demand
  ;; :custom
  ;; (prescient-filter-method '(literal regexp initialism fuzzy))
  ;; (prescient-sort-length-enable nil)
  :config
  (prescient-persist-mode +1))

(use-package orderless
  :disabled
  :custom
  (completion-styles '(orderless basic)))
