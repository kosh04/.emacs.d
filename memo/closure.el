;;; memo/closure -*- lexical-binding: t -*-

;; Emacs Lisp におけるクロージャ / 静的束縛について

;; 参考リンク

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Closures.html
;; http://mbork.pl/2016-05-17_Emacs_Lisp_closures_demystified

(let ((counter 0))
  (defun memo:increase-counter (&optional x)
    (cl-incf counter (or x 1)))
  (defun memo:get-counter ()
    counter))

(let ((print-circle t))
  (list (symbol-function 'memo:increase-counter)
        (symbol-function 'memo:get-counter)))

;; =>
;; ((closure #1=((counter . 0) t)
;;           (&optional x)
;;           (setq counter (+ counter (or x 1))))
;;  (closure #1#
;;           nil
;;           counter))

;; クロージャ変数のconsセルは共有しているため
;; 2つの関数のどちらからでも同じクロージャ変数を参照できる

(cl-defstruct closure
  name                                  ; `closure'
  env                                   ; クロージャ変数 (連想リスト?)
  args                                  ; 引数
  body                                  ; 本体
  )

(require 'cl)
(lexical-let ((count 0))
  (defun memo:counter ()
    (incf count)))

;; (memo:counter) ;=> 1
;; (memo:counter) ;=> 2

;; lexical-binding の前進(?)となる `lexical-let' もやっていることはほとんど同じ
;; (symbol-function 'memo:counter)
;; ->
;; (lambda (&rest --cl-rest--)
;;   (apply '(closure
;;            ((--cl-count-- . --count--) t)
;;            (G13101)
;;            (let* ((v G13101))
;;              (set v (1+ (symbol-value G13101)))))
;;          '--count-- --cl-rest--))
