;;; m3u-mode.el --- Major mode for edit m3u/m3u8 format playlist.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2018  KOBAYASHI Shigeru (kosh)

;; Author: KOBAYASHI Shigeru <shigeru.kb@gmail.com>
;; Keywords: multimedia
;; Version: 0.1-beta
;; Created: 2018-01-21
;; URL: https://localhost/
;; Package-Requires: ((emacs "24.4"))

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

;; *TODO*

;; HTTP Live Streaming
;; https://www.rfc-editor.org/rfc/rfc8216.txt

;;; Code:

(defconst m3u-mode-font-lock-keywords
  (list
   ;;'("^#\\(EXTM3U\\)$" 1 font-lock-keyword-face t)
   ;; #EXTINF:<duration>,[<title>]
   '("^#\\(EXTINF\\):\\(-?[0-9]+\\.?[0-9]*\\),\\(.*\\)$" ; ? "ArtistName - TrackTitle"
     (1 'font-lock-keyword-face t)
     (2 'italic t)
     (3 'bold t))
   (list (rx bol "#" (submatch
                      (or
                       "EXTM3U"
                       "EXT-X-VERSION"   ; #EXT-X-VERSION:<n>
                       "EXT-X-BYTERANGE" ; #EXT-X-BYTERANGE:<n>[@<o>]
                       "EXT-X-KEY"       ; #EXT-X-KEY:<attribute-list>
                       "EXT-X-MAP"       ; #EXT-X-MAP:<attribute-list>
                       "EXT-X-PROGRAM-DATE-TIME" ; #EXT-X-PROGRAM-DATE-TIME:<YYYY-MM-DDThh:mm:ss.SSSZ>
                       "EXT-X-DATERANGE" ; #EXT-X-DATERANGE:<attribute-list>
                       "EXT-X-GAP"       ; #EXT-X-GAP
                       "EXT-X-TARGETDURATION" ; #EXT-X-TARGETDURATION:<s>
                       "EXT-X-MEDIA-SEQUENCE" ; #EXT-X-MEDIA-SEQUENCE:<number>
                       "EXT-X-DISCONTINUITY-SEQUENCE" ; #EXT-X-DISCONTINUITY-SEQUENCE:<number>
                       "EXT-X-ENDLIST"                ; #EXT-X-ENDLIST
                       "EXT-X-DISCONTINUITY" ; #EXT-X-DISCONTINUITY
                       "EXT-X-PLAYLIST-TYPE" ; #EXT-X-PLAYLIST-TYPE:<EVENT|VOD>
                       "EXT-X-I-FRAMES-ONLY" ; #EXT-X-I-FRAMES-ONLY
                       "EXT-X-MEDIA"      ; #EXT-X-MEDIA:<attribute-list>
                       "EXT-X-STREAM-INF" ; #EXT-X-STREAM-INF:<attribute-list>\n<URI>
                       )))
         '(1 'font-lock-keyword-face t))

   ;; <attribute-lists>: AttributeName=AttributeValues
   (list (rx (submatch
              (1+ (char upper digit ?-)))
             "="
             (submatch
              (or (: (or "0x" "0X" ) (1+ hex))      ; hexadecimal-sequence
                  (: (1+ digit) "x" (1+ digit))     ; decimal-resolution
                  (: (? (or "+" "-")) (1+ digit) (? "." (1+ digit))) ; decimal-integer, decimal-floating-point
                  (: "\"" (0+ (not (any "\"" "\r" "\n"))) "\"")      ; quoted-string
                  (: (1+ (not (any "\"" "," " ")))) ; enumerated-string
                  )))
         '(1 'underline t)
         '(2 'font-lock-constant-face t))
   ))

(defconst m3u-mode-syntax-table
  (let ((table (make-syntax-table text-mode-syntax-table)))
    (modify-syntax-entry ?\# "<   " table)
    (modify-syntax-entry ?\n ">   " table)
    table))

;;;###autoload
(define-derived-mode m3u-mode text-mode "M3U"
  "Major mode for edit m3u/m3u8 format playlist."
  :syntax-table m3u-mode-syntax-table
  (setq font-lock-defaults '(m3u-mode-font-lock-keywords))
  (setq comment-start "#" comment-end "")
  (setq comment-use-syntax t)           ; ?
  (setq imenu-generic-expression
        (list
         '(nil "^#\\(EXTINF\\):\\(-?[0-9]+\\.?[0-9]*\\),\\(.*\\)$" 3)))
  (setq imenu-case-fold-search nil)
  (goto-address-mode))

;;;###autoload
(progn
  (add-to-list 'auto-mode-alist '("\\.m3u8?\\'" . m3u-mode))
  (add-to-list 'file-coding-system-alist `("\\.m3u8\\'" . utf-8))

  (with-eval-after-load 'autoinsert
    (or (assoc "\\.m3u8?\\'" (symbol-value 'auto-insert-alist))
        (define-auto-insert "\\.m3u8?\\'" '(_ "#EXTM3U\n"))))
  )

(defun m3u-new-entry (title path &optional length)
  (format "#EXTINF:%d,%s\n%s" (or length -1) title path))

(defun m3u-insert-entry (title path)
  "Insert M3U Entry."
  (interactive "*sTitle: \nsPath: ")
  (insert (m3u-new-entry title path) "\n"))

(provide 'm3u-mode)
;;; m3u-mode.el ends here
