;;; config/windows-conf.el

;; Font
(set-face-attribute 'default nil :family "Consolas" :height 110)
;; (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "MeiryoKe_Console"))
;; (setq face-font-rescale-alist '(("MeiryoKe_Console" . 1.08)))

;; NOTE: gnutls.c: [1] Note that the security level of the
;; Diffie-Hellman key exchange has been lowered to 256 bits and this
;; may allow decryption of the session data
(with-eval-after-load "gnutls"
  (setq gnutls-min-prime-bits 1024))

;; NTEmacs64 IME-patched
;; https://github.com/chuntaro/NTEmacs64

(with-eval-after-load 'w32-ime
  (setq default-input-method "W32-IME")
  (setq-default w32-ime-mode-line-state-indicator "[－]")
  (setq w32-ime-mode-line-state-indicator-list '("[－]" "[あ]" "[－]"))
  (w32-ime-initialize)
  (add-hook 'w32-ime-on-hook #'(lambda () (set-cursor-color "brown")))
  (add-hook 'w32-ime-off-hook #'(lambda () (set-cursor-color "black")))
  ;;(add-hook 'minibuffer-setup-hook 'deactivate-input-method)
  )

;; VC-Git
;; 外部プロセスを抑制する
(with-eval-after-load 'vc-hooks
  ;;(setq vc-handled-backends (delete 'Git vc-handled-backends))
  (remove-hook 'find-file-hook 'vc-find-file-hook)
  (remove-hook 'kill-buffer-hook 'vc-kill-buffer-hook))

;; プチフリーズはGCが原因かもしれない
;;(setq gc-cons-threshold 800000 gc-cons-percentage 0.1)
(setq gc-cons-threshold (* gc-cons-threshold 5))
(setq gc-cons-percentage 0.5)

;; image-dired
(custom-set-variables
 ;; 公式のバイナリを拾ってくるか、以下のmagick.exeを利用する
 ;; http://opensourcepack.blogspot.jp/p/converter.html
 '(image-dired-cmd-create-thumbnail-program "magick convert"))

;; cygwinにパスを通したくないけど
;; diredのy(ファイルタイプ判別)ではfileコマンドを利用したい
(defun user:with-cygwin-path (f &rest args)
  (let ((exec-path  exec-path)
        (cygwin/bin (expand-file-name "bin" (getenv "CYGWIN_HOME"))))
    (push cygwin/bin exec-path)
    (apply f args)))

(advice-add 'dired-show-file-type :around #'user:with-cygwin-path)

;; TODO: Replace with `ag' or `git-grep' command?
(custom-set-variables
 ;; M-x lgrep
 '(grep-template "findstr /N /R <R> <F>")
 ;; M-x rgrep
 '(grep-find-template "findstr /S /N /R /D:<D> <R> <F>")
 '(find-name-arg nil))

;; etags を使いたいだけ
(let ((path #1=(getenv "PATH"))
      (bindir (expand-file-name "../../../../bin" exec-directory)))
  (unless (string-match bindir path)
    (setf #1# (concat path path-separator bindir))))

(with-eval-after-load 'ispell
  (custom-set-variables
   '(ispell-program-name "c:/msys64/mingw64/bin/aspell.exe")))

(with-eval-after-load 'doc-view
  (add-to-list 'exec-path "c:/opt/gs9.21/bin")
  (set-variable 'doc-view-ghostscript-program (executable-find "gswin64c")))

(with-eval-after-load 'curl
  (setq request-curl-options '("--insecure")))
