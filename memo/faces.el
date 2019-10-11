;;; memo/faces.el

(list-colors-display)
;; "/usr/lib/X11/rgb.txt"

(defun list-hexadecimal-colors-display ()
  (interactive)
  (let ((r 0) (g 0) (b 0) (delta 32)
        (color-list ()))
    (while (< r 256)
      (setq g 0)
      (while (< g 256)
        (setq b 0)
        (while (< b 256)
          (push (format "#%.2x%.2x%.2x" r g b) color-list)
          (setq b (+ b delta)))
        (setq g (+ g delta)))
      (setq r (+ r delta)))
    (list-colors-display color-list)))

(face-list)
;; M-x: describe-face が既にあった
(defun face-describe (face)
  `((:id ,(face-id face))
    (:modified ,(face-differs-from-default-p face))
    (:nontrivial ,(face-nontrivial-p face))
    (:foreground ,(face-foreground face))
    (:background ,(face-background face))
    (:stipple ,(face-stipple face))
    (:underline ,(face-underline-p face))
    (:inverse-video ,(face-inverse-video-p face))
    (:bold ,(face-bold-p face))
    (:italic ,(face-italic-p face))
    (:font ,(face-font face))
    (:docstring ,(face-documentation face))
    ))
(describe-face 'dired-new-modified)

;; GNU Emacs Color Theme Test - C
;; カラーテーマの見本
;; http://www.cs.cmu.edu/~maverick/GNUEmacsColorThemeTest/index-c.html

;; カーソル位置のfaceを調べる関数
(defun describe-face-at-point()
  "Return facce used at point."
  (interactive)
  (message "%s" (get-char-property (point) 'face)))

(face-font 'default)
;;=> "-outline-Consolas-normal-normal-normal-mono-15-*-*-*-c-*-iso8859-1"

;; 上書き可能な defface (C-M-x `eval-defun' で代用可能)
;; http://paste.lisp.org/display/90293
(defmacro defface! (face spec doc &rest args)
  `(custom-declare-face-1 ',face ,spec ,doc ,@args))

(defun custom-declare-face-1 (face spec doc &rest args)
  "Like `defface', but FACE is evaluated as a normal argument."
  (progn                            ; (unless (get face 'face-defface-spec)
    (when (fboundp 'facep)
      (progn                        ; (unless (facep face)
        (let ((value (or (get face 'saved-face) spec)))
          (make-empty-face face)
          (dolist (frame (frame-list))
            (face-spec-set face value frame)))
        (if (memq window-system '(x w32 mac))
            (make-face-x-resource-internal face)))
      ;; Don't record SPEC until we see it causes no errors.
      (put face 'face-defface-spec spec)
      (push (cons 'defface face) current-load-list)
      (when (and doc (null (face-documentation face)))
        (set-face-documentation face (purecopy doc)))
      (custom-handle-all-keywords face args 'custom-face)
      (run-hooks 'custom-define-hook)))
  face)

(defface url '((t (:inherit link)))
  "URLのface"
  :group 'font-lock-faces)
(setf (get 'url 'face-alias) 'url-face)

;; dark-mode, light-mode 切り替え
;; あるいは起動オプションの --reverse-video, -r, rv
(custom-set-variables
 '(frame-background-mode 'dark))
