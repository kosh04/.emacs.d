;;; config/Info-ja

;; Emacs 25.1 (+24.5) 日本語マニュアル
;; https://ayatakesi.github.io/
;; https://github.com/ayatakesi/emacs-25.1-doc-emacs

(add-to-list 'Info-directory-list "~/Dropbox/Downloads/editor/emacs")

(defun Info-find-node--info-ja (fn filename &rest args)
  "emacs.infoへの参照を日本語Infoにする.
URL `http://rubikitch.com/2016/07/06/emacs245-manual-ja/'"
  (let ((filename* (pcase filename
                     ("emacs" "emacs-25.1-ja")
                     (_ filename))))
    (apply fn filename* args)))
(advice-add 'Info-find-node :around 'Info-find-node--info-ja)
