;; https://github.com/radian-software/straight.el
;; https://www.emacswiki.org/emacs/StraightEl

;; ディレクトリ構成
;; ~/.emacs.d/straight
;; build/
;; repos/

;; Bootstrap install
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'el-patch)
(straight-use-package 'git)
(straight-use-package 'use-package)

(setq straight-use-package-by-default t)
