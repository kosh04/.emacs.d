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

;; toggle [?\C-x ?t]
(global-set-key (kbd "C-x t e") 'toggle-debug-on-error)
(global-set-key (kbd "C-x t f") 'toggle-truncate-lines)
(global-set-key (kbd "C-x t p") 'list-packages)
(global-set-key (kbd "C-x t v") 'toggle-viper-mode)
(global-set-key (kbd "C-x t w") 'whitespace-mode)
(global-set-key (kbd "C-x t o") 'ff-find-related-file)
(global-set-key (kbd "C-x t n") 'display-line-numbers-mode)

(defun user::toggle-url-debug ()
  (interactive)
  (let ((bufname "*URL-DEBUG*"))
    (if url-debug
        (progn
          ;; restore windows configuration
          (let ((val (get 'url-debug 'current-window-configuration)))
            (if val (set-window-configuration val)))
          (if (find-buffer bufname)
              (kill-buffer bufname)))
      (progn
        ;; save window configuration
        (setf (get 'url-debug 'current-window-configuration) (current-window-configuration))
        (display-buffer (get-buffer-create bufname)))))
  (setq url-debug (not url-debug))
  (message "url-debug=%s" url-debug))
(global-set-key (kbd "C-x t u") 'user::toggle-url-debug)

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

(global-set-key (kbd "<f1> A") #'apropos)
