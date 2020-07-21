memo/calc
=========

GNU Emacs Calc -- 高機能な電卓/数式処理ソフト

## Link

- http://www.gnu.org/software/emacs/manual/calc.html
- http://www.gnu.org/software/emacs/refcards/pdf/calccard.pdf [PDF]
- https://www.emacswiki.org/emacs/Calc
- https://florian.adamsky.it/2016/03/31/emacs-calc-for-programmers-and-cs.html
- https://github.com/ahyatt/emacs-calc-tutorials

## 基本的な使い方

PRN(逆ポーランド記法)電卓を起動する (M-x `calc`)

    C-x * C

一行電卓を起動する (M-x `quick-calc`)

    C-x * q

スタンドアロンアプリとして利用する

    $ emacs -f full-calc

## 扱える数と記法 (C-h f `calc-mode`)

- 実数		10, 3.14e6, _23 (-23), 17:3 (17/3)
- ベクタ		[1, 2, 3]
- 行列		[[1,2], [3,4]]
- 複素数		(1, 2) = 1+2i
- 範囲		[1 .. 4) equal 1<= x < 4

### 計算例

    exp((pi/4)i)
    RPN: ( 0 P 4 / ) E = (0.707106781186, 0.707106781187)

## キーコマンド (とりあえずこれだけは覚えておけ)

- <kbd>U</kbd> : `calc-undo`
- <kbd>y</kbd> : `calc-copy-to-buffer`
- <kbd>'</kbd> : 入力をクォートする. 数式を一行で書きたいときに.

### 基数を変更する

- <kbd>d 2</kbd> 2進数 (Binary)
- <kbd>d 8</kbd> 8進数 (Octal)
- <kbd>d 0</kbd> 10進数 (Decimal)
- <kbd>d 6</kbd> 16進数 (Hexadecimal)
- <kbd>d r</kbd> 基数を選択する (2-36)

### 6.8 Language Modes

表示方法いろいろ

- d N : ノーマル記法 (`calc-normal-language`)
  - 例 `sqrt((a + 1) / b + c^2)`
- d O : ?
- d B : 分数記法
- d U : 演算子をすべて関数扱いする (`calc-unformatted-language`)
  - 例 `sqrt(add(div(add(a, 1), b), pow(c, 2)))`
- d C : Cプログラム記法
  - 例 `sqrt((a + 1) / b + pow(c, 2))`
- d P : Pascal記法
- d F : Fortran記法
- d T : Tex記法
- d L : LaTex記法
- d E : TROFFテキスト記法？
- d Y : Yacas記法
- d X : Maxima記法
- d A : Giac記法
- d M : Mathmatica記法
- d W : Maple記法

### 6.9 Modes変数

[_コマンド_] _説明_ : _デフォルト値_

1. [p] 精度 (precisition) : 12
2. [b w] バイナリ長 (binary word size): 32
3. [m g] スタックサイズ
4. [d r] 基数 (radix / 2~36) : 10
5. [d s] 浮動小数点フォーマット (Floating-point format)
6. [m d] 度数モード (Angular mode) : 1 degree (, 2 radians, 3 HMS)
7. [m s] シンボリックモード (srqt等の評価を後回しにする) : 0
8. [m f] Fraction mode : 0
9. [m p] Polar mode : 0
10. [m v] Matrix/Scalar mode : -1 (0:Scalar mode, -2:Matrix mode, -3:square Matrix mode, N:NxN Matrix mode)
11. [m D?] Simplification mode : 1
12. [m i] Infinite mode : -1 (off)

### 6.10 モードライン

    --%*-Calc: 12 Deg other modes   (Calculator)

### 7.1 算術関数
### 7.2 整数の切り捨て
### 7.3 複素数

`calc-imaginary` : 数値を複素数として扱う.

    例: 123 calc-imaginary => (0, 123)

### 7.4 換算 (Conversions)
### 7.5 日付
### 7.6 会計・金融
### 7.7 二進数

ビット演算

- b a : `calc-and` AND
- b o : `calc-or`  OR
- b x : `calc-xor` XOR
- b n : `calc-not` NOT
- b d : `calc-diff` Bitwise difference. diff(2#1010, 2#1100) = 2#0100
- b r : `calc-rshift-binary` : 論理右シフト rsh(a,n) = lsh(a,-n)
- b l : `calc-lshift-binary` : 論理左シフト lsh(a, 1)
- b R : `calc-rshift-arith` : 算術右シフト rash()
- b L : `calc-lshift-arith` : 算術左シフト lash()
- b t : `calc-rotate-binary` : ビット・ローテーション

## 8. 科学
### 8.5 乱数

- k r : `calc-random`

### 8.6 組み合わせ (Combinatorial Functions)

- k f : `calc-prime-factors` 複素数分解

    2006 k f => [2, 17, 59]

- k n : `calc-next-prime`

    2003 k n => 2011

- k p : `calc-prime-test` 素数判定

## 9. 配列と行列 (Vector/Matric Functions)
## 10. 代数 (Algebra)
## 11. 単位の計算 (Operating on Units)
## 12. 値の保存 (Storing and Recalling)
## 13. Graphics

gnuplot に出力できるらしい？

## 14. 切り取りと貼り付け (Kill and Yank Functions)
## 15. 電卓モード (Keypad Mode)
## 16. Embedded Mode
## 17. プログラミング (Programming)

LISPで数式処理ができる点ではMaximaとよく似ている.

## Tips

http://www.eonet.ne.jp/~3alchemists/Calc/Texinfo/Texi2html/calc-jp_30.html

練習問題 13 ハッシュコードの計算

```calc
. ' "Testing, 1, 2, 3" RET

=> [84, 101, 115, 116, 105, 110, 103, 44, 32, 49, 44, 32, 50, 44, 32, 51]

. V R ' 3$$+$ RET

=> 1960915098
```

同じことを Emacs Lisp でやると以下のようになる

```emacs-lisp
(seq-reduce (lambda (acc elt)
              (+ (* 3 acc) elt))
            "Testing, 1, 2, 3"
            0)
;=> 1960915098
```
