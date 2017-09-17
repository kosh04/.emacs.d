;;; zalgo.el --- Zalgo text generator                -*- lexical-binding: t; -*-

;; Copyright (C) 2017 kosh

;; Author:  KOBAYASHI Shigeru (kosh) <shigeru.kb@gmail.com>
;; Keywords: i18n
;; License: MIT

;;; Commentary:

;; [TODO]

;; Link:
;; - http://megaemoji.com/generators/zalgo/

;;

;;; Code:

(require 'cl-lib)

(defconst zalgo-chars--up
  [?\u030D ?\u030E ?\u0304 ?\u0305 ?\u033F ?\u0311 ?\u0306 ?\u0310 ?\u0352 ?\u0357
   ?\u0351 ?\u0307 ?\u0308 ?\u030A ?\u0342 ?\u0313 ?\u0308 ?\u0301 ?\u034A ?\u034B ?\u034C
   ?\u0303 ?\u0302 ?\u030C ?\u0350 ?\u0300 ?\u0301 ?\u030B ?\u030F ?\u0312 ?\u0313
   ?\u0314 ?\u033D ?\u0309 ?\u0363 ?\u0364 ?\u0365 ?\u0366 ?\u0367 ?\u0368 ?\u0369
   ?\u036A ?\u036B ?\u036C ?\u036D ?\u036E ?\u036F ?\u033E ?\u035B ?\u0346 ?\u031A])

(defconst zalgo-chars--down
  [?\u0316 ?\u0317 ?\u0318 ?\u0319 ?\u031C ?\u031D ?\u031E ?\u031F ?\u0320 ?\u0324
   ?\u0325 ?\u0326 ?\u0329 ?\u032A ?\u032B ?\u032C ?\u032D ?\u032E ?\u032F ?\u0330
   ?\u0331 ?\u0332 ?\u0333 ?\u0339 ?\u033A ?\u033B ?\u033C ?\u0345 ?\u0347 ?\u0348
   ?\u0349 ?\u034D ?\u034E ?\u0353 ?\u0354 ?\u0355 ?\u0356 ?\u0359 ?\u035A ?\u0323])

(defconst zalgo-chars--middle
  [?\u0315 ?\u031B ?\u0300 ?\u0301 ?\u0358 ?\u0321 ?\u0322 ?\u0327 ?\u0328 ?\u0334
   ?\u0335 ?\u0336 ?\u034F ?\u035C ?\u035D ?\u035E ?\u035F ?\u0360 ?\u0362 ?\u0337
   ?\u0361 ?\u0489])

(defconst zalgo-chars
  `((up     . ,zalgo-chars--up)
    (down   . ,zalgo-chars--down)
    (middle . ,zalgo-chars--middle)))

(defconst zalgo-char-table
  (let ((tbl (make-hash-table)))
    (dolist (cs (list zalgo-chars--up
                      zalgo-chars--middle
                      zalgo-chars--down))
      (mapc (lambda (c) (puthash c t tbl)) cs))
    tbl))

(defvar-local zalgo-overlays nil)

(defsubst zalgo--gen (s depth)
  '(with-output-to-string
    (dotimes (_ depth)
      (princ (string (aref s (random (length s)))))))
  (apply #'string (cl-loop repeat depth collect (aref s (random (length s)))))
  )

(defsubst zalgo--corrupter ()
  (concat (zalgo--gen zalgo-chars--up 2)
          (zalgo--gen zalgo-chars--middle 1)
          (zalgo--gen zalgo-chars--down 5)))

(defun zalgo-new (text)
  (replace-regexp-in-string "[[:ascii:]]"
                            (lambda (s)
                              (propertize s 'zalgo-corrupter (zalgo--corrupter)))
                            text t t))

(defvar-local zalgo-overlays nil)

(defun zalgo-regeion (start end)
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (while (re-search-forward (rx alnum) nil t)
        ;;(replace-match (concat (match-string 0) (zalgo--corrupter)) nil t)
        (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
          (setf (overlay-get ov 'display) (concat (match-string 0) (zalgo--corrupter)))
          (push ov zalgo-overlays))
        ))))

(defun zalgo-clear ()
  (interactive)
  (mapc #'delete-overlay zalgo-overlays))

(require 'zone)

(defun zone-pgm-zalgo ()
  (zone-call (lambda ()
               )))

(provide 'zalgo)
;;; zalgo.el ends here
