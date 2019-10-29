;;; config/Info-ja

;; Emacs 日本語マニュアル
;; https://ayatakesi.github.io/
;; https://github.com/ayatakesi/emacs-25.1-doc-emacs

;; ~/.emacs.d/share/info/*.info
(add-to-list 'Info-directory-list (locate-user-emacs-file "share/info"))

(defun Info-find-node--info-ja (fn filename &rest args)
  "emacs.infoへの参照を日本語Infoにする.
URL `http://rubikitch.com/2016/07/06/emacs245-manual-ja/'"
  (let ((filename* (pcase filename
                     ("emacs" "emacs-ja")
                     ("elisp" "elisp-ja")
                     (_ filename))))
    (apply fn filename* args)))

(when (or (locate-file "emacs-ja.info" Info-directory-list)
          (locate-file "emacs-ja.info.gz" Info-directory-list))
  (advice-add 'Info-find-node :around 'Info-find-node--info-ja))
