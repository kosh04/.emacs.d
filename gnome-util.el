;;; gnome-util.el --- Call GNOME application utility	-*- encoding:utf-8 -*-

(defun nautilus (&optional dir)
  "Open nautilus - the GNOME File Manager"
  (interactive "DNautilus: ")
  (call-process "nautilus" nil 0 nil
                (expand-file-name (or dir default-directory))))

(defun gnome-terminal ()
  "Open GNOME Terminal"
  (interactive)
  (call-process "gnome-terminal" nil 0 nil default-directory))

(defun charmap ()
  "GNOME 文字マップ"
  (interactive)
  (call-process "charmap" nil 0))

;; or
;; (start-process "charmap" nil "/usr/bin/charmap")

;; (call-process "xdg-open" nil nil nil "http://www.newlisp.org/")
;; (case (call-process "xdg-open" nil nil nil "~/ChangeLog")
;;   (0 "Exit success.")
;;   (1 "Error in command line syntax.")
;;   (2 "One of the files passed on the command line did not exist.")
;;   (3 "A required tool could not be found")
;;   (4 "The action failed."))
;; browse-url-kde-program                  ;=> "kfmclient"
(defun shell-execute (filename &optional directory params)
  "FILENAMEを関連付けられたプログラムで開く."
  (let ((default-directory (or directory default-directory)))
    (call-process "gnome-open" nil 0 nil (expand-file-name filename))))

(provide 'gnome-util)

;;; gnome-util.el
