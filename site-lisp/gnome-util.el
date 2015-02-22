;;; gnome-util.el --- GNOME application utility	-*- encoding:utf-8 -*-

(defun nautilus (&optional dir)
  "Open nautilus - the GNOME File Manager."
  (interactive "DNautilus: ")
  (call-process "nautilus" nil 0 nil
                (expand-file-name
                 (or dir default-directory))))

(defun gnome-terminal ()
  "Open GNOME Terminal."
  (interactive)
  (call-process "gnome-terminal" nil 0 nil default-directory))

(defun charmap ()
  "GNOME 文字マップ."
  (interactive)
  (call-process "charmap" nil 0))

;; or (start-process "charmap" nil "/usr/bin/charmap")

(defvar open-program-name
  (if (executable-find "xdg-open")
      "xdg-open"
      "gnome-open"))

(defun shell-execute (filename &optional directory params)
  "FILENAMEを関連付けられたプログラムで開く."
  (setq filename (expand-file-name filename))
  (let ((default-directory (or directory default-directory)))
    (apply #'call-process open-program-name
           nil 0 nil filename params)))

(provide 'gnome-util)

;;; gnome-util.el
