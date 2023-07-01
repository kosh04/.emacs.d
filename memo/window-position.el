;;; memo/window-position.el

(window-inside-edges)                   ; (1 1 107 39)

;; マウスポジション
;; フレームを外れると nil になるらしい
(list (cdr (mouse-pixel-position))
      (cdr (mouse-position)))
;;=> ((587 . 269) (73 . 16))

;; ウィンドウポジション、高さと幅
(list (cons (x-display-pixel-height) (x-display-pixel-width))
      (cons (x-display-mm-height) (x-display-mm-width))
      (posn-x-y (posn-at-point))
      (cons (window-height) (window-width)))
;;=> ((900 . 1600) (318 . 564) (352 . 303) (36 . 101))

;; (window area-or-pos (x . y) timestamp object pos (col . row) image (dx . dy) (width . height))
(destructuring-bind (0posn-window
                     1posn-area
                     2posn-x-y
                     3posn-timestamp
                     4posn-string
                     5posn-point
                     6posn-actual-col-row
                     7posn-image
                     8posn-object-x-y
                     9posn-object-width-height)
    (posn-at-point))

;; フレームポジション
(list (cons (frame-pixel-height) (frame-pixel-width))
      (cons (frame-height) (frame-width))
      (cons (frame-char-height) (frame-char-width)))

;; カーソルポジション
(list (posn-col-row (posn-at-point))
      (posn-actual-col-row (posn-at-point))
      (cons (window-start) (window-end))
      (cons (point-min) (point-max)))

;; フレームのサイズと位置
;; frame-{height,width} の古い別名
(cons (screen-height) (screen-width))

;; 各ウィンドウには次の属性があります
;; * ウィンドウを含んでいるフレーム
;; * ウィンドウの高さ
;; * ウィンドウの幅
;; * スクリーンやフレームを基準にしたウィンドウの隅
;; * ウィンドウが表示しているバッファ
;; * ウィンドウの左上隅に対応するバッファ内の位置
;; * コラム単位の水平方向のスクロール量
;; * ポイント
;; * マーク
;; * どの程度最近にウィンドウが選択されたか

;; 左端--上端--右端--下端
(window-edges (selected-window))        ; (0 0 84 20)  上側のウィンドウ
(window-edges (selected-window))        ; (0 20 84 41) 下側のウィンドウ
;; 下端が**行目であるのは、最下行はエコー領域だからである。

;; +-------------+
;; |(0 0)        |
;; |             |
;; +-------------+(84 20)
;; |(0 20)       |
;; |             |
;; +-------------+(84 41)

;;              column 35
;; +------------+-------------------+ line 0
;; |(0 0 35 15) |(35 0 80 15)       |
;; +------------+-------------------+ line 15
;; |(0 15 80 50)                    |
;; |                                |
;; +--------------------------------+ line 50
;; column 0                         column 80

;; 座標とウィンドウ
(window-at 0 0 (selected-frame))        ; #<window 3 on *info*>
(coordinates-in-window-p '(0 . 0) (selected-window))

(current-frame-configuration)
(frame-parameters)
(current-window-configuration)

(move-to-window-line -1)
(recenter -1)

(defun recenter-top () (interactive) (recenter 0))
(defun recenter-bottom () (interactive) (recenter -1))

;; 最大化
(modify-frame-parameters (selected-frame) '((fullscreen . maximized)))

;; ?
(setq switch-to-buffer-obey-display-actions t)
