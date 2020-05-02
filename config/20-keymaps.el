;;; config/keymaps

(global-set-key (kbd "C-h") 'backward-delete-char)
(define-key isearch-mode-map (kbd "C-h") 'isearch-delete-char)

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

;; Keymap for user-defined toggle commands.
(defalias 'ctl-c-t-prefix
  (let ((map (make-sparse-keymap)))
    (define-key map "e" 'toggle-debug-on-error)
    (define-key map "f" 'toggle-truncate-lines)
    (define-key map "p" 'list-packages)
    (define-key map "v" 'toggle-viper-mode)
    (define-key map "w" 'whitespace-mode)
    (define-key map "o" 'ff-find-related-file)
    (define-key map "n" 'display-line-numbers-mode)
    (define-key map "u" 'user::toggle-url-debug)
    (define-key map "s" 'user::toggle-scratch-buffer)
    (define-key map "l" 'ielm)
    map))
(global-set-key (kbd "C-c t") 'ctl-c-t-prefix)

;;(global-set-key [f11] 'toggle-frame-fullscreen)
(global-set-key [f11] 'toggle-frame-maximized)

;; disable
(global-unset-key (kbd "C-x m"))        ; compose-mail
(global-unset-key (kbd "C-x C-n"))      ; set-goal-column

(define-key completion-list-mode-map "q"
  (lambda () (interactive) (quit-window t)))

;; prefixキーのコマンド一覧を表示
(use-package which-key
  :diminish which-key-mode
  :hook (emacs-startup . which-key-mode)
  :custom (which-key-idle-delay 1.5))

(with-eval-after-load 'xref
  (let ((map xref--xref-buffer-mode-map))
    (define-key map (kbd "TAB") 'xref-next-line)
    (define-key map (kbd "DEL") 'xref-prev-line)))

;; or C-u q (in view-mode)
;;(global-set-key (kbd "C-x k") 'kill-this-buffer)

(global-set-key (kbd "<f1> A") 'apropos)

;;(global-set-key (kbd "M-o") 'other-window)

;; FIXME
;; ReadOnly を切り替えることが多いので C-x C-q 以外のキーが欲しい (evil?)
(global-set-key (kbd "ESC M-q") 'read-only-mode)
;; WindowsTerminal v0.11.1121 にて C-@ が反応しないためその場しのぎな対処
(global-set-key (kbd "ESC M-@") 'set-mark-command)
