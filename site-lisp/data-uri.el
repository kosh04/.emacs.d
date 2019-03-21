;;; data-uri.el --- Create data URI scheme           -*- lexical-binding: t; -*-

;; Copyright (C) 2018  KOBAYASHI Shigeru (kosh)

;; Author: KOBAYASHI Shigeru <shigeru.kb@gmail.com>
;; Created: 2018-08-20
;; Version: 0.1.0-git
;; Package-Requires: ((emacs "24.5"))
;; Keywords: convenience, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; - RFC2397
;; - https://developer.mozilla.org/ja/docs/data_URIs

;;; Exmample:
;;
;; (data-uri "./data-uri.el") ;;=> "data:application/emacs-lisp;base64,Ozs7IGR..."
;; (data-uri shell-file-name) ;;=> "data:application/octet-stream;base64,TVqQAAM..."
;; (data-uri "emacs.png")     ;;=> "data:image/png;base64,iVBORw0K..."

;;; Code:

(require 'mailcap)

(defun data-uri--url-encode (data &optional mime charset)
  (when (multibyte-string-p data)
    (setq charset (or charset (coding-system-get (detect-coding-string data t) :mime-charset))
          data (encode-coding-string data 'raw-text)))
  (when charset
    (setq mime (format "%s;charset=%s" (or mime "text/plain") charset)))
  (format "data:%s,%s"
          (or mime "")
          (url-hexify-string data)))

(defun data-uri--base64-encode (data &optional mime data-nobr)
  (when (multibyte-string-p data)
    (setq data (encode-coding-string data 'raw-text)))
  (format "data:%s;base64,%s"
          (or mime "")
          (base64-encode-string data data-nobr)))

;;;###autoload
(defun data-uri (file &optional mime data-nobr)
  "Create data URI shcheme from FILE."
  (let ((data (with-temp-buffer
                ;;(set-buffer-multibyte t)
                (insert-file-contents-literally file)
                (buffer-string))))
    (unless mime
      (setq mime (mailcap-extension-to-mime
                  (or (file-name-extension file) ""))))
    (if (and mime (string-match mime "\\`text/"))
        (data-uri--url-encode data mime)
      (data-uri--base64-encode data mime data-nobr))))

;;;###autoload
(defun data-uri-insert-file (file)
  (interactive "fInsert data URI from file: ")
  (insert (data-uri file)))

(provide 'data-uri)
;;; data-uri.el ends here
