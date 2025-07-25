;;; config/keymaps

;; NOTE: "C-c LETTER" はユーザー用に予約済み (キーバインディング規約より)
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Conventions.html
;; https://ayatakesi.github.io/lispref/29.1/html/Key-Binding-Conventions.html

(global-set-key (kbd "C-h") 'backward-delete-char)
(define-key isearch-mode-map (kbd "C-h") 'isearch-delete-char) ; or isearch-del-char

(global-set-key (kbd "C-l") 'recenter)

(global-set-key [remap just-one-space] 'cycle-spacing)

;; [C-x C-v] 等は通常通り使いたいので全部載せはいらない
;;(require 'ffap)
;;(ffap-bindings)
;;(global-set-key (kbd "C-x C-f") 'find-file-at-point)

(global-set-key (kbd "C-c C-o") 'browse-url-at-point)

(defun user::toggle-url-debug ()
  (interactive)
  (let ((bufname "*URL-DEBUG*"))
    (if url-debug
        (progn
          ;; restore windows configuration
          (let ((val (get 'url-debug 'current-window-configuration)))
            (if val (set-window-configuration val)))
          (if (get-buffer bufname)
              (kill-buffer bufname)))
      (progn
        ;; save window configuration
        (setf (get 'url-debug 'current-window-configuration) (current-window-configuration))
        (display-buffer (get-buffer-create bufname)))))
  (setq url-debug (not url-debug))
  (message "url-debug=%s" url-debug))

;;(define-prefix-command 'ctl-c-t-map)
(defalias 'ctl-c-t-map
  (let ((map (make-sparse-keymap "Toggle")))
    (define-key map "e" '("debug" . toggle-debug-on-error))
    (define-key map "f" '("fold". toggle-truncate-lines)) ; or visual-line-mode
    (define-key map "p" '("packages" . list-packages))
    (define-key map "P" '("packages" . package-list-packages-no-fetch))
    (define-key map "v" '("viper" . toggle-viper-mode))
    (define-key map "w" '("whitespace" . whitespace-mode))
    (define-key map "o" '("find-related" . ff-find-related-file))
    (define-key map "n" '("line-numbers" . display-line-numbers-mode))
    (define-key map "u" '("url-debug" . user::toggle-url-debug))
    (define-key map "s" '("scratch" . scratch-buffer))
    (define-key map "l" '("ielm" . ielm))
    map)
  "Keymap for user-defined toggle commands.")
(global-set-key (kbd "C-c t") '("Toggle any" . ctl-c-t-map))

;; TODO: 上記のキー定義を bind-keys, hydra, defvar-keymap で書き換える？

'
(bind-keys
 :prefix-map ctl-c-t-map
 :prefix "C-c t"
 :prefix-docstring "Keymap for user-defined toggle commands."
 ("e" . toggle-debug-on-error)
 ("f" . toggle-truncate-lines)
 )

;; (defhydra hydra-toggle (global-map "C-c t")
;;   ("s" scratch-buffer "scratch")
;;   )

;;(global-set-key [f11] 'toggle-frame-fullscreen)
(global-set-key [f11] 'toggle-frame-maximized)

;; disable
(global-unset-key (kbd "C-x m"))        ; compose-mail
(global-unset-key (kbd "C-x C-n"))      ; set-goal-column

;; (define-key completion-list-mode-map "q"
;;   (lambda () (interactive) (quit-window t)))

(defun switch-to-last-buffer ()
  "Display last visited buffer."
  (interactive)
  (switch-to-buffer nil))
(global-set-key (kbd "C-x l") #'switch-to-last-buffer)

;; prefixキーのコマンド一覧を表示
;; (define-key MAP KEY (DESC . DEFUN)) が定義されていて役割が被っている場合はどうする？
(use-package which-key
  :diminish which-key-mode
  :hook (emacs-startup . which-key-mode)
  :custom
  (which-key-idle-delay 1.5)
  ;; FIXME: 値は設定されるが `which-key-separator' が nil になってしまう (カスタム変数の :set が機能していない？)
  ;;(which-key-dont-use-unicode nil)
  )

;; or C-u q (in view-mode)
;;(global-set-key (kbd "C-x k") 'kill-this-buffer)
;;(global-set-key [remap kill-buffer] #'kill-current-buffer)

(global-set-key (kbd "<f1> A") 'apropos)

;;(global-set-key (kbd "M-o") 'other-window)

;; FIXME
;; ReadOnly を切り替えることが多いので C-x C-q 以外のキーが欲しい (evil?)
(global-set-key (kbd "ESC M-q") 'read-only-mode)
;; WindowsTerminal v0.11.1121 にて C-@ が反応しないためその場しのぎな対処
(global-set-key (kbd "ESC M-@") 'set-mark-command)

(use-package bind-key
  :bind
  (("<f1> =" . describe-personal-keybindings)
   ("<f1> S" . shortdoc)))

(global-set-key (kbd "C-x C-,") 'switch-to-prev-buffer)
(global-set-key (kbd "C-x C-.") 'switch-to-next-buffer)

;; https://github.com/kickingvegas/casual
