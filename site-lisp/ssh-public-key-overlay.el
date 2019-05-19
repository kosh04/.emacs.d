;;; ssh-public-key-overlay.el --- Provide SSH Public Key overlay -*- lexical-binding: t; -*-

;; Copyright (C) 2017  KOBAYASHI Shigeru

;; Author: KOBAYASHI Shigeru (kosh[04]) <shgeru.kb@gmail.com>
;; Version: 0.1snapshot
;; Created: 2017-04-18
;; Package-Requires: ((emacs "24.3") (names "20151201.0") (rainbow-identifier "0.2.2"))
;; Keywords: ssh, tools
;; License: MIT
;; URL: https://github.com/kosh04/.emacs.d/blob/master/site-lisp/ssh-public-key-overlay.el

;;; Commentary:

;; [TODO]
;; - names

;;; Code:

(eval-when-compile
  (require 'rx)
  (require 'names))
(require 'rainbow-identifiers)

;;;###autoload
(define-namespace ssh-public-key-overlay-

(defconst regexp
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

;; (defvar-local -overlays nil)
(defvar -overlays nil nil)
(make-variable-buffer-local '-overlays)

(defun default-format (s)
  (format "%s...#%d" (substring s 0 (1- 32)) (length s)))

(defvar format #'default-format
  "Function")

(defun enable ()
  "Enable ssh-public-key overlay."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (let* ((n 2) ;; match[N]
             (ov (make-overlay (match-beginning n) (match-end n)))
             (s  (match-string n))
             (ss (funcall format s))
             (hash (rainbow-identifiers--hash-function s))
             (face (rainbow-identifiers-cie-l*a*b*-choose-face hash)))
        (push ov -overlays)
        (setf (overlay-get ov 'display) ss)
        (setf (overlay-get ov 'face) `((:underline t) ,@face))
        (setf (overlay-get ov 'mouse-face) 'highlight
              (overlay-get ov 'help-echo) s)
        )))
  (message (substitute-command-keys
            "Public key is omitted. Type \\[read-only-mode] to edit.")))

(defun disable ()
  "Disable ssh-public-key overlay."
  (mapc 'delete-overlay -overlays)
  (setq -overlays nil))

(defun -ro ()
  (if buffer-read-only
      (enable)
    (disable)))

:autoload
(defun mode ()
  (interactive)
  (add-hook 'read-only-mode-hook #'mode nil 'local)
  (if (called-interactively-p 'interactive)
      (read-only-mode +1)
    (-ro)))

) ;; end namespace

;; optional
(eval-after-load 'ssh-config-mode
  '(progn
     (add-hook 'ssh-authorized-keys-mode-hook #'ssh-public-key-overlay-mode)
     (add-hook 'ssh-known-hosts-mode-hook     #'ssh-public-key-overlay-mode)))

(provide 'ssh-public-key-overlay)

;;; ssh-public-key-overlay.el ends here
