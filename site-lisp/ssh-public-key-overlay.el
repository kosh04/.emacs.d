;;; ssh-public-key-overlay.el --- Provide SSH Public Key overlay -*- lexical-binding: t; -*-

;; Copyright (C) 2017  KOBAYASHI Shigeru

;; Author: KOBAYASHI Shigeru (kosh[04]) <shgeru.kb@gmail.com>
;; Version: 0.3-git
;; Created: 2017-04-18
;; Package-Requires: ((emacs "28.1"))
;; Keywords: ssh, tools
;; License: MIT
;; URL: https://github.com/kosh04/.emacs.d/blob/master/site-lisp/ssh-public-key-overlay.el

;;; Commentary:

;;; ChangeLog:
;; v0.3 [2025-10-31] Replace short namespace `names' with `read-symbol-shorthands'

;;; Code:

(eval-when-compile
  (require 'rx))

(defconst so-regexp
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

(defvar-local so--overlays nil)

(defun so-format (s)
  (format "%s...#%d" (substring s 0 (1- 32)) (length s)))

(defun so-enable ()
  "Enable ssh-public-key overlay."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward so-regexp nil t)
      (let* ((n 2) ;; match[N]
             (ov (make-overlay (match-beginning n) (match-end n)))
             (s  (match-string n))
             ;; unique-like color name from text object
             (color (let* ((colors (defined-colors))
                           (n (mod (sxhash s) (length colors))))
                      (nth n colors))))
        (push ov so--overlays)
        (setf (overlay-get ov 'display) (so-format s))
        (setf (overlay-get ov 'face) `(:underline t :background ,color))
        (setf (overlay-get ov 'mouse-face) 'highlight
              (overlay-get ov 'help-echo) (format "text:%s\ncolor:%s" s color))
        ;; do u like rainbow?
        ;; (require 'rainbow-identifiers)
        ;; (let* ((hash (rainbow-identifiers--hash-function s))
        ;;        (face (rainbow-identifiers-cie-l*a*b*-choose-face hash)))
        ;;   (setf (overlay-get ov 'face) `((:underline t) ,@face)))
        )))
  (message (substitute-command-keys
            "Public key is omitted. Type \\[read-only-mode] to edit.")))

(defun so-disable ()
  "Disable ssh-public-key overlay."
  (mapc 'delete-overlay so--overlays)
  (setq so--overlays nil))

(defun so-maybe ()
  (if buffer-read-only
      (so-enable)
    (so-disable)))

;;;###autoload
(define-minor-mode so-mode
  "[TODO]DOC"
  :init-value nil
  (cond (so-mode
	 (add-hook 'read-only-mode-hook 'so-maybe t 'local)
	 (so-maybe))
	(t
	 (remove-hook 'read-only-mode-hook 'so-maybe 'local)
	 (so-disable))))

(with-eval-after-load 'ssh-config-mode
  (add-hook 'ssh-authorized-keys-mode-hook #'so-mode)
  (add-hook 'ssh-known-hosts-mode-hook     #'so-mode))

(provide 'ssh-public-key-overlay)

;; Local Variables:
;; read-symbol-shorthands: (("so-" . "ssh-public-key-overlay-"))
;; End:

;;; ssh-public-key-overlay.el ends here
