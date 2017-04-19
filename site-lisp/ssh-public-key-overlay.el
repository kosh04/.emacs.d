;;; ssh-public-key-overlay.el --- Provide SSH Public Key overlay -*- lexical-binding: t; -*-

;; Copyright (C) 2017  KOBAYASHI Shigeru

;; Author: KOBAYASHI Shigeru (kosh) <shgeru.kb@gmail.com>
;; Version: 0.1snapshot
;; Created: 2017-04-18
;; Package-Requires: ((emacs "24.3"))
;; Keywords: ssh, tools
;; License: MIT

;;; Commentary:

;; [TODO]

;;; Code:

(eval-when-compile
  (require 'rx))

(defconst ssh-public-key-overlay-regexp
  (rx
   ;; KEY TYPE
   (submatch                            ; match[1]
    (or "ecdsa-sha2-nistp256"
        "ecdsa-sha2-nistp384"
        "ecdsa-sha2-nistp521"
        "ssh-ed25519"
        "ssh-dss"
        "ssh-rsa"))
   (+ space)
   ;; BASE64-ENCODED KEY
   (submatch                            ; match[2]
    (: (* (= 4 #1=(or alnum "+" "/")))
       (or (: (= 2 #1#) "==")
           (: (= 3 #1#) "=")
           (: (= 4 #1#)))))
   (? (+ space) 
      ;; COMMENT
      (submatch (* graphic)))           ; match[3]
   eol)
  "OpenSSH Public Key format.")

(defvar-local ssh-public-key-overlay--overlays nil)

(defun ssh-public-key-overlay-enable ()
  "Enable ssh-public-key overlay."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward ssh-public-key-overlay-regexp nil t)
      (let* ((n 2) ;; match[N]
             (ov (make-overlay (match-beginning n) (match-end n)))
             (s  (match-string n))
             (ss (format "%s...#%d" (substring s 0 7) (length s))))
        (push ov ssh-public-key-overlay--overlays)
        (setf (overlay-get ov 'display) ss)
        (setf (overlay-get ov 'face) "underline")
        (setf (overlay-get ov 'mouse-face) 'highlight
              (overlay-get ov 'help-echo) s)
        )))
  (message (substitute-command-keys
            "Public key is omitted. Type \\[read-only-mode] to edit.")))

(defun ssh-public-key-overlay-disable ()
  "Disable ssh-public-key overlay."
  (mapc 'delete-overlay ssh-public-key-overlay--overlays)
  (setq ssh-public-key-overlay--overlays nil))

(defun ssh-public-key-overlay--ro ()
  (if buffer-read-only
      (ssh-public-key-overlay-enable)
    (ssh-public-key-overlay-disable)))

;;;###autoload
(defun ssh-public-key-overlay-mode ()
  (interactive)
  (add-hook 'read-only-mode-hook #'ssh-public-key-overlay-mode nil 'local)
  (if (called-interactively-p 'interactive)
      (read-only-mode +1)
    (ssh-public-key-overlay--ro)))

;; optional
(eval-after-load 'ssh-config-mode
  '(progn
     (add-hook 'ssh-authorized-keys-mode-hook #'ssh-public-key-overlay-mode)
     (add-hook 'ssh-known-hosts-mode-hook     #'ssh-public-key-overlay-mode)))

(provide 'ssh-public-key-overlay)

;;; ssh-public-key-overlay.el ends here
