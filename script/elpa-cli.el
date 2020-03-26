#!/usr/bin/env emacs

(unless noninteractive
  (error "Only scripting"))

(require 'package)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
	("melpa" . "https://melpa.org/packages/")
	("melpa-stable" . "https://stable.melpa.org/packages/"))
      package-archive-priorities
      '(("gnu" . 20)
        ("melpa-stable" . 10)
        ("melpa". 0))
      ;; package-pinned-packages ()
      )

(defun printf (fmt &rest args)
  (princ (apply #'format fmt args)))

(defun usage ()
  (printf "Usage: %s --script %s COMMAND ARGS...\n"
	  invocation-name (file-name-nondirectory #$)))

(package-initialize)
(package-refresh-contents)

(defun elpa-cli-cmd/list ()
  (list-packages)
  (with-current-buffer "*Packages*"
    (printf "%s" (buffer-string))))

(defun elpa-cli-cmd/info (pkg)
  (describe-package (intern pkg))
  (with-current-buffer "*Help*"
    (printf "%s" (buffer-string))))

(defun elpa-cli-cmd/install (pkgs)
  (dolist (pkg pkgs)
    (package-install (intern pkg))))

(defun elpa-cli-cmd/search (regexp)
  (list-packages)
  (with-current-buffer "*Packages*"
    (occur regexp))
  (with-current-buffer "*Occur*"
    (printf "%s" (buffer-string))))

(defun elpa-cli-cmd/update ()
  (warn "NOT TESTED YET")
  (list-packages)
  (package-menu-mark-upgrades)
  (package-menu-execute))

(defun elpa-cli-cmd/lint ()
  (error "TODO: see %s" "package-lint.el"))

(defun main (args)
  (pcase args
    (`("list")
     (elpa-cli-cmd/list))
    (`("info" ,pkg)
     (elpa-cli-cmd/info pkg))
    (`("install" . ,pkgs)
     (elpa-cli-cmd/install pkgs))
    (`("update")
     (elpa-cli-cmd/update))
    (`("search" ,regexp)
     (elpa-cli-cmd/search regexp))
    (`("lint" . ,_)
     ;;(require 'package-lint)
     (pop command-line-args-left)
     (package-lint-batch-and-exit))
    (_ (usage))))

(main command-line-args-left)
