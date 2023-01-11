;; https://github.com/progfolio/elpaca

;; Elpaca is an elisp package manager. It allows users to find,
;; install, update, and remove third-party packages for Emacs.
;; It is a replacement for the built-in Emacs package manager, package.el.

;; bootstrap
(declare-function elpaca-generate-autoloads "elpaca")
(defvar elpaca-directory (locate-user-emacs-file "elpaca/"))
(defvar elpaca-builds-directory (locate-user-emacs-file "elpaca/builds/"))
(when-let ((elpaca-repo (expand-file-name "repos/elpaca/" elpaca-directory))
           (elpaca-build (expand-file-name "elpaca/" elpaca-builds-directory))
           (elpaca-target (if (file-exists-p elpaca-build) elpaca-build elpaca-repo))
           (elpaca-url  "https://www.github.com/progfolio/elpaca.git")
           ((add-to-list 'load-path elpaca-target))
           ((not (file-exists-p elpaca-repo)))
           (buffer (get-buffer-create "*elpaca-bootstrap*")))
  (condition-case-unless-debug err
      (progn
        (unless (zerop (call-process "git" nil buffer t "clone" elpaca-url elpaca-repo))
          (error "%s" (list (with-current-buffer buffer (buffer-string)))))
        (byte-recompile-directory elpaca-repo 0 'force)
        (require 'elpaca)
        (elpaca-generate-autoloads "elpaca" elpaca-repo)
        (kill-buffer buffer))
    ((error)
     (delete-directory elpaca-directory 'recursive)
     (with-current-buffer buffer
       (goto-char (point-max))
       (insert (format "\n%S" err))
       (display-buffer buffer)))))
(require 'elpaca-autoloads)
(add-hook 'after-init-hook #'elpaca-process-queues)
;(funcall #'elpaca-process-queues)

(elpaca (elpaca :host github :repo "progfolio/elpaca"))

;; M-x elpaca-manager
;; t - `elpaca-status' インストールしたもの一覧

(elpaca-use-package use-package)

(elpaca use-package
  (require 'use-package))
(elpaca-use-package evil
  :demand t)

;; TODO: キューに入ったパッケージをインストールする方法
