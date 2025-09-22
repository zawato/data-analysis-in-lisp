# Lispでデータ分析をやってみる（２） - 基本統計量の計算

# はじめに
[前回](../step1/step1.md)は、環境のセットアップとCSVファイルの読み込み方法を実装しました。今回は、読み込んだデータを使って基本統計量（平均、中央値、分散、標準偏差）を計算する関数を実装していきます。

普段Pythonでデータ分析をしている身としては、`numpy`や`pandas`を使って`df.mean()`、`df.median()`、`df.var()`、`df.std()`などの簡単な関数呼び出しで済む処理を、Lispでどう実装するか考えるのは興味深い課題です。Pythonでの経験を活かしながら、Lispの関数名や使い方もできるだけPythonに近づけてみましょう。

# 基本統計量の実装
## 平均値（Mean）
まずは最も基本的な統計量である平均値を計算する関数を実装します。Pythonの`np.mean()`や`pandas.Series.mean()`に相当する関数です：

```lisp
;;; 平均値を計算する関数
(defun mean (numbers)
  "数値リストの平均値を計算する（Pythonのnp.mean/pandas.meanに相当）"
  (/ (reduce #'+ numbers) (length numbers)))
```

## 中央値（Median）
次に、中央値を計算する関数を実装します。中央値はデータを昇順に並べた時の中央の値（データ数が偶数の場合は中央の2つの値の平均）です。Pythonの`np.median()`や`pandas.Series.median()`に相当します：

```lisp
;;; 中央値を計算する関数
(defun median (numbers)
  "数値リストの中央値を計算する（Pythonのnp.median/pandas.medianに相当）"
  (let* ((sorted (sort (copy-seq numbers) #'<))
         (len (length sorted))
         (mid (floor len 2)))
    (if (oddp len)
        (nth mid sorted)
        (/ (+ (nth (1- mid) sorted) (nth mid sorted)) 2))))
```

## 分散（Variance）
分散は、データの散らばり具合を示す指標です。Pythonの`np.var()`や`pandas.Series.var()`に相当します：

```lisp
;;; 分散を計算する関数
(defun variance (numbers)
  "数値リストの分散を計算する（Pythonのnp.var/pandas.varに相当）"
  (let* ((avg (mean numbers))
         (squared-diffs (mapcar (lambda (x) (expt (- x avg) 2)) numbers)))
    (mean squared-diffs)))
```

## 標準偏差（Standard Deviation）
標準偏差は分散の平方根です。Pythonの`np.std()`や`pandas.Series.std()`に相当します：

```lisp
;;; 標準偏差を計算する関数
(defun std (numbers)
  "数値リストの標準偏差を計算する（Pythonのnp.std/pandas.stdに相当）"
  (sqrt (variance numbers)))
```

## 最小値と最大値
データの範囲を把握するための最小値と最大値も実装しましょう：

```lisp
;;; 最小値を取得する関数
(defun min-value (numbers)
  "数値リストの最小値を返す（Pythonのmin/pandas.minに相当）"
  (reduce #'min numbers))

;;; 最大値を取得する関数
(defun max-value (numbers)
  "数値リストの最大値を返す（Pythonのmax/pandas.maxに相当）"
  (reduce #'max numbers))
```

# 四分位数とデータ要約
Pythonの`pandas.describe()`のような機能を実装するために、四分位数を計算する関数も実装しましょう：

```lisp
;;; 四分位数を計算する関数
(defun quantile (numbers q)
  "数値リストのq分位点を計算する（Pythonのnp.quantile/pandas.quantileに相当）"
  (let* ((sorted (sort (copy-seq numbers) #'<))
         (len (length sorted))
         (pos (* q (1- len)))
         (pos-floor (floor pos))
         (pos-ceil (ceiling pos)))
    (if (= pos-floor pos-ceil)
        (nth pos-floor sorted)
        (+ (* (- pos pos-floor) (nth pos-ceil sorted))
           (* (- pos-ceil pos) (nth pos-floor sorted))))))

;;; 記述統計量をまとめて計算する関数
(defun describe (numbers)
  "数値リストの記述統計量を計算する（Pythonのpandas.describeに相当）"
  (list :count (length numbers)
        :mean (mean numbers)
        :std (std numbers)
        :min (min-value numbers)
        :25% (quantile numbers 0.25)
        :50% (median numbers)
        :75% (quantile numbers 0.75)
        :max (max-value numbers)))
```

# 実装したコードのテスト
前回作成したCSVファイルから読み込んだデータを使って、実装した関数をテストしてみましょう。前回実装したPythonライクなCSV読み込み関数を活用します：

```lisp
;;; 基本統計量のテスト
(defun test-statistics ()
  (format t "~%基本統計量のテスト:~%")
  (let* ((df (make-df-from-csv "../step1/height_weight.csv" '(1 2)))
         (heights (mapcar (lambda (row) (nth 1 row)) (dataframe-data df)))
         (weights (mapcar (lambda (row) (nth 2 row)) (dataframe-data df))))
      
    (format t "身長データ: ~A~%" heights)
    (format t "  平均値: ~,2F cm~%" (mean heights))
    (format t "  中央値: ~,2F cm~%" (median heights))
    (format t "  分散: ~,2F~%" (variance heights))
    (format t "  標準偏差: ~,2F cm~%" (std heights))
      
    (format t "~%体重データ: ~A~%" weights)
    (format t "  平均値: ~,2F kg~%" (mean weights))
    (format t "  中央値: ~,2F kg~%" (median weights))
    (format t "  分散: ~,2F~%" (variance weights))
    (format t "  標準偏差: ~,2F kg~%~%" (std weights))
    
    ;; pandas.describeのような出力
    (format t "~%pandas.describeのような出力:~%")
    (let ((height-stats (describe heights))
          (weight-stats (describe weights)))
      (format t "身長の統計量:~%")
      (loop for (key value) on height-stats by #'cddr
            do (format t "  ~A: ~,2F~%" key value))
      (format t "~%体重の統計量:~%")
      (loop for (key value) on weight-stats by #'cddr
            do (format t "  ~A: ~,2F~%" key value)))))
```

# データフレームの列ごとの統計量計算
実際のデータ分析では、データフレーム全体の各列に対して統計量を計算したいことがよくあります。そこで、Pythonの`df.describe()`のように、データフレームの列ごとに統計量を計算する関数も実装してみましょう：

```lisp
;;; データフレームの列に対して統計量を計算する関数
(defun df-describe (df &optional columns)
  "データフレームの指定列に対して記述統計量を計算する（Pythonのdf.describeに相当）"
  (let* ((headers (dataframe-headers df))
         (data (dataframe-data df))
         (cols (or columns (loop for i from 1 below (length headers) collect i)))
         (results nil))
    (dolist (col cols results)
      (let* ((col-name (nth col headers))
             (col-data (mapcar (lambda (row) (nth col row)) data))
             (stats (describe col-data)))
        (push (cons col-name stats) results)))))
```

# 完全なコード例
上記のすべての機能を組み合わせた完全なコード例は以下のようになります：

```lisp
;;; pandas_stats.lisp - Pythonのpandas統計関数に相当する実装

;; CSV読み込み関数を読み込む
(load "../step1/read_csv.lisp")

;;; 平均値を計算する関数
(defun mean (numbers)
  "数値リストの平均値を計算する（Pythonのnp.mean/pandas.meanに相当）"
  (/ (reduce #'+ numbers) (length numbers)))

;;; 中央値を計算する関数
(defun median (numbers)
  "数値リストの中央値を計算する（Pythonのnp.median/pandas.medianに相当）"
  (let* ((sorted (sort (copy-seq numbers) #'<))
         (len (length sorted))
         (mid (floor len 2)))
    (if (oddp len)
        (nth mid sorted)
        (/ (+ (nth (1- mid) sorted) (nth mid sorted)) 2))))

;;; 分散を計算する関数
(defun variance (numbers)
  "数値リストの分散を計算する（Pythonのnp.var/pandas.varに相当）"
  (let* ((avg (mean numbers))
         (squared-diffs (mapcar (lambda (x) (expt (- x avg) 2)) numbers)))
    (mean squared-diffs)))

;;; 標準偏差を計算する関数
(defun std (numbers)
  "数値リストの標準偏差を計算する（Pythonのnp.std/pandas.stdに相当）"
  (sqrt (variance numbers)))

;;; 最小値を取得する関数
(defun min-value (numbers)
  "数値リストの最小値を返す（Pythonのmin/pandas.minに相当）"
  (reduce #'min numbers))

;;; 最大値を取得する関数
(defun max-value (numbers)
  "数値リストの最大値を返す（Pythonのmax/pandas.maxに相当）"
  (reduce #'max numbers))

;;; 四分位数を計算する関数
(defun quantile (numbers q)
  "数値リストのq分位点を計算する（Pythonのnp.quantile/pandas.quantileに相当）"
  (let* ((sorted (sort (copy-seq numbers) #'<))
         (len (length sorted))
         (pos (* q (1- len)))
         (pos-floor (floor pos))
         (pos-ceil (ceiling pos)))
    (if (= pos-floor pos-ceil)
        (nth pos-floor sorted)
        (+ (* (- pos pos-floor) (nth pos-ceil sorted))
           (* (- pos-ceil pos) (nth pos-floor sorted))))))

;;; 記述統計量をまとめて計算する関数
(defun describe (numbers)
  "数値リストの記述統計量を計算する（Pythonのpandas.describeに相当）"
  (list :count (length numbers)
        :mean (mean numbers)
        :std (std numbers)
        :min (min-value numbers)
        :25% (quantile numbers 0.25)
        :50% (median numbers)
        :75% (quantile numbers 0.75)
        :max (max-value numbers)))

;;; データフレームの列に対して統計量を計算する関数
(defun df-describe (df &optional columns)
  "データフレームの指定列に対して記述統計量を計算する"
  (let* ((headers (dataframe-headers df))
         (data (dataframe-data df))
         (cols (or columns (loop for i from 1 below (length headers) collect i)))
         (results nil))
    (dolist (col cols results)
      (let* ((col-name (nth col headers))
             (col-data (mapcar (lambda (row) (nth col row)) data))
             (stats (describe col-data)))
        (push (cons col-name stats) results)))))

;;; 基本統計量のテスト
(defun test-statistics ()
  (format t "~%基本統計量のテスト:~%")
  (let* ((df (make-df-from-csv "../step1/height_weight.csv" '(1 2)))
         (heights (mapcar (lambda (row) (nth 1 row)) (dataframe-data df)))
         (weights (mapcar (lambda (row) (nth 2 row)) (dataframe-data df))))
      
    (format t "身長データ: ~A~%" heights)
    (format t "  平均値: ~,2F cm~%" (mean heights))
    (format t "  中央値: ~,2F cm~%" (median heights))
    (format t "  分散: ~,2F~%" (variance heights))
    (format t "  標準偏差: ~,2F cm~%" (std heights))
      
    (format t "~%体重データ: ~A~%" weights)
    (format t "  平均値: ~,2F kg~%" (mean weights))
    (format t "  中央値: ~,2F kg~%" (median weights))
    (format t "  分散: ~,2F~%" (variance weights))
    (format t "  標準偏差: ~,2F kg~%~%" (std weights))
    
    ;; pandas.describeのような出力
    (format t "~%pandas.describeのような出力:~%")
    (let ((stats (df-describe df '(1 2))))
      (dolist (col-stat stats)
        (format t "~%列: ~A~%" (car col-stat))
        (loop for (key value) on (cdr col-stat) by #'cddr
              do (format t "  ~A: ~,2F~%" key value))))))

;; メイン関数
(defun main ()
  (format t "~%Lispデータ分析 - Step 2: Pythonライクな統計関数実装~%")
  (test-statistics))

;; プログラム実行
(main)
```

# Pythonとの比較
Pythonでは、同様の処理が以下のように簡潔に書けます：

```python
import pandas as pd

# CSVファイルを読み込む
df = pd.read_csv('height_weight.csv')

# 基本統計量を計算
print("身長の平均値:", df['height'].mean())
print("身長の中央値:", df['height'].median())
print("身長の分散:", df['height'].var())
print("身長の標準偏差:", df['height'].std())

# 全ての列の基本統計量をまとめて計算
print(df.describe())
```

Pythonでは`pandas`という強力なライブラリがあるため、たった数行で同じ処理が実現できます。一方、Lispでは各統計量の計算ロジックを自分で実装する必要がありますが、その過程で統計量の計算方法を深く理解できるというメリットがあります。

また、Lispの関数型プログラミングの特徴を活かすことで、データの変換や処理を柔軟に行うことができます。例えば、`mapcar`関数を使って列データを抽出したり、`reduce`関数を使って合計や最大値を求めたりする処理は、Lispの得意とするところです。

# おわりに
今回はPythonの`pandas`や`numpy`に相当する基本統計量を計算する関数を実装しました。Lispの関数型プログラミングの特徴を活かした実装ができ、Pythonでの経験を活かしながらLispの理解も深めることができました。

## 次回予告
次回は、これらのデータを可視化する方法を探っていきます。Lispには標準でグラフ描画機能がないため、外部ツールとの連携方法を検討します。

Pythonでは`matplotlib`や`seaborn`を使って簡単にグラフを描画できますが、Lispではどのように実現するのか、その過程でLispの特徴を活かした実装方法を探っていきましょう。Pythonでのデータ可視化の経験を活かしながら、Lispならではの方法を見つけていきたいと思います。