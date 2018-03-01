;;; textproc.el --- Text processing tool            -*- lexical-binding: t; -*-

;; Provide `textproc-filter-region' command to text conversion on current region.

;; Version: 0.1
;; Created: 2016-05-16
;; Keywords: convenience, wp

;;; Commentary:

;; (textproc-filter-region #'upcase START END) ~= (upcase-region START END)

;; テキスト変換関数いろいろ
;; (ただし、リージョン指定＆変換コマンドは標準で提供されていることが多い)

;; - url (%XX) (`url-hexify-string')
;; - uudecode (`uudecode-decode-region')
;; - base64  (`base64-decode-region')
;; - charset (`encode-coding-region')
;; - unicode (\uXXXX)
;; - htmlentities
;; - 半角 全角 (`japanese-hankaku' / `japanese-zenkaku')
;; - ひらがな カタカナ (`japanese-hiragana' / `japanese-katakana')
;; - 漢数字 ローマ数字 アラビア数字

;; Related Command:

;; - `call-process-region'
;; - `shell-command-on-region' ("M-|")

;;; Code:

;;;###autoload
(defun textproc-filter-region (command start end)
  "Convert text on current region by using filter COMMAND.
COMMAND requires method ((input string)) -> string."
  (interactive "*aFilter command: \nr")
  ;; FIXME: 変換後のポイント位置がずれる
  (let ((text (delete-and-extract-region start end)))
    (insert (funcall command text))))

;; (save-restriction
;;   (narrow-to-region start end)
;;   (save-excursion
;;     (goto-char (point-min))
;;     (while (re-search-forward REGEXP nil t)
;;       (replace-match NEWTEXT nil t))))

(defun textproc-filter-string (regionf string)
  "Convert STRING by using filter REGIONF.
REGIONF requires method ((beg point) (end point))."
  (with-temp-buffer
    (insert string)
    (funcall regionf (point-min) (point-max))
    (buffer-string)))

;; Example:

;;(require 'url-util)
;;(require 'morse)

;; urlencode
(defun textproc-urlencode-region (start end)
  (interactive "*r")
  (textproc-filter-region #'url-hexify-string start end))

(defun textproc-urldecode-region (start end)
  (interactive "*r")
  (textproc-filter-region #'url-unhex-string start end))

;; html/xml
(defun textproc-parse-html (string)
  (textproc-filter-string (lambda (start end)
                    (let ((dom (libxml-parse-html-region start end)))
                      (delete-region start end)
                      (insert (dom-texts dom))))
                  string))

;; morse
(defun textproc-morse-string (string)
  (textproc-filter-string #'morse-region string))

(defun textproc-unmorse-string (string)
  (textproc-filter-string #'unmorse-region string))

(defvar textproc-filter-commands
  '(md5 sha1))

(defmacro textproc-define-command (command)
  "Define command `textproc-COMMAND-region'."
  (let ((name (intern (format "textproc-%s-region" command))))
    `(defun ,name (start end)
       (interactive "*r")
       (textproc-filter-region #',command start end))))

(textproc-define-command md5)
(textproc-define-command sha1)

(provide 'textproc)

;;; textproc.el ends here
