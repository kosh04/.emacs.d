;;; config/Emacs Develop

;; Emacs Source Repository
;; - https://git.savannah.gnu.org/cgit/emacs.git
;; - https://www.emacswiki.org/emacs/GitForEmacsDevs

(with-eval-after-load 'find-func
  ;; NOTE: 野良ビルドした場合は`source-directory'が有効になる
  (unless find-function-C-source-directory
    (setq find-function-C-source-directory
          (cond ((file-directory-p #1="~/src/gitrepo/emacs/src/") #1#)
                (t
                 ;; 常に最新版を取得するため参照するソース元が一致しないかも
                 ;; (format "http://git.savannah.gnu.org/cgit/emacs.git/plain/src/?h=emacs-%s" emacs-version)
                 (format "https://github.com/emacs-mirror/emacs/raw/emacs-%s/src/" emacs-version))))))

;; GNU Bug Tracker
;; https://debbugs.gnu.org
;; https://www.emacswiki.org/emacs/EmacsBugTracker
;; M-x `debbugs-gnu'
;; M-x `debbugs-gnu-search'
;; M-x `debbugs-gnu-manual'
;; M-x `report-emacs-bug'
;; M-x `gnus-read-ephemeral-emacs-bug-group'
(use-package debbugs
  :defer t
  :config
  (add-hook 'debbugs-gnu-mode-hook #'hl-line-mode))
