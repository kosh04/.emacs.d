;;; m3u-mode.el --- Major mode for edit m3u/m3u8 format playlist.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  KOBAYASHI Shigeru (kosh04)

;; Author: KOBAYASHI Shigeru <shigeru.kb@gmail.com>
;; Keywords: multimedia

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; TODO:

;; - imenu 対応

;;; Code:

(require 'cl-lib)

(defconst m3u-mode-font-lock-keywords
  (list
   '("^#\\(EXTM3U\\)$" 1 font-lock-keyword-face t)
   '("^#\\(EXTINF\\):\\(-?[0-9]+\\),\\(.*\\)$" ; ? "ArtistName - TrackTitle"
     (1 'font-lock-keyword-face t)
     (2 'italic t)
     (3 'highlight t))
   ;;'("^#.*$" 0 font-lock-comment-face nil)
   ))

(defvar m3u-mode-syntax-table
  (let ((tbl (copy-syntax-table text-mode-syntax-table)))
    (modify-syntax-entry ?# "<   " tbl)
    tbl))

(define-derived-mode m3u-mode text-mode "M3U"
  "Major mode for edit m3u/m3u8 format playlist."
  :syntax-table m3u-mode-syntax-table
  (setq font-lock-defaults '(m3u-mode-font-lock-keywords))
  ;;(font-lock-add-keywords nil m3u-mode-font-lock-keywords)
  (setq comment-start "#" comment-end "")
  (goto-address-mode))

(defconst m3u-mode-ext-regexp "\\.m3u8?\\'")

(add-to-list 'auto-mode-alist `(,m3u-mode-ext-regexp . m3u-mode))

;; autoinsert
(define-auto-insert m3u-mode-ext-regexp
  '(_ "#EXTM3U\n"))

;; M3U8
(add-to-list 'file-coding-system-alist `("\\.m3u8\\'" . utf-8))

;;;###TODO

(cl-defstruct (m3u-entry)
  "M3U Entry Spec."
  (title  :type string)
  (length :type number)
  (path   :type url))

(defun m3u-add-new-entry (title path)
  "Add M3U Entry."
  (interactive "sTitle: \nsPath: ")
  (insert (format "#EXTINF:%d,%s\n" -1 title))
  (insert path))

(provide 'm3u-mode)
;;; m3u-mode.el ends here
