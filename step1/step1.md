# Lispでデータ分析をやってみる （１） 環境構築とCSVファイルの読み込み

# はじめに
普段はPythonでデータ分析をしている私ですが、最近Lispを勉強し始めました。せっかくなので、Lispの学習を兼ねてデータ分析をLispで実装してみようと思います。このシリーズでは、Pythonでのデータ分析経験を活かしながら、同等の処理をLispでどう実装するかを探っていきます。

第1回となる今回は、環境のセットアップとCSVファイルの読み込みを実装します。Pythonでは`pandas.read_csv()`一行で済む処理を、Lispではどのように実装するのか、そしてその過程でLispの特徴をどう活かせるのかを見ていきましょう。

# 環境のセットアップ
## 環境情報
このシリーズでは、Common Lispの実装の一つであるSBCL (Steel Bank Common Lisp) を使用します。

- OS: macOS (またはLinux/Windows)
- SBCL: バージョン2.3.0以上
- Quicklisp: ライブラリ管理ツール

## 導入手順
### SBCLのインストール
macOSの場合、Homebrewを使って簡単にインストールできます：

```bash
brew install sbcl
```

Linuxの場合：

```bash
# Ubuntuの場合
sudo apt-get install sbcl

# Fedoraの場合
sudo dnf install sbcl
```

Windowsの場合は、[公式サイト](http://www.sbcl.org/platform-table.html)からインストーラをダウンロードしてください。

### Quicklispのインストール
Quicklispは、Common Lispのライブラリ管理システムです。以下の手順でインストールします：

1. Quicklispのインストールスクリプトをダウンロード：
```bash
curl -O https://beta.quicklisp.org/quicklisp.lisp
```

2. SBCLを起動してQuicklispをロード：
```bash
sbcl --load quicklisp.lisp
```

3. SBCL内で以下を実行：
```lisp
(quicklisp-quickstart:install)
(ql:add-to-init-file)
(quit)
```

## 動作確認
SBCLが正しくインストールされたか確認するために、簡単なHello Worldプログラムを実行してみましょう：

```lisp
(defun hello-world ()
  (format t "Hello, Lisp Data Analysis World!~%"))

(hello-world)
```

このコードをファイル（例：`hello.lisp`）に保存し、以下のコマンドで実行します：

```bash
sbcl --script hello.lisp
```

または、SBCLを対話モードで起動し、コードを直接入力することもできます：

```bash
sbcl
* (defun hello-world ()
    (format t "Hello, Lisp Data Analysis World!~%"))
* (hello-world)
```

「Hello, Lisp Data Analysis World!」と表示されれば、環境のセットアップは完了です。

# CSVファイルの読み込み
Pythonでは、pandasを使って`df = pd.read_csv('data.csv')`のように簡単にCSVファイルを読み込めますが、Lispではどうでしょうか？普段Pythonでデータ分析をしている身としては、できるだけPythonに近い使い勝手を実現したいところです。

## サンプルデータの準備
まずは分析用の簡単なCSVファイルを作成します。身長と体重のデータを含む`height_weight.csv`を作成しましょう：

```csv
id,height,weight
1,170,65
2,175,70
3,165,60
4,180,80
5,160,55
```

## Pythonライクなread_csv関数の実装
Common Lispには標準でCSV読み込み機能がないため、自分で実装する必要があります。Pythonのpandasに近い使い勝手を目指して、以下のようなCSVパーサーを実装してみました：

```lisp
;;; read_csv.lisp - Pythonのpandas.read_csvに相当する機能の実装

;; 文字列をカンマで分割する関数
(defun split-by-comma (line)
  (loop for i = 0 then (1+ j)
        for j = (position #\, line :start i)
        collect (subseq line i j)
        until (null j)))

;; CSVファイルを読み込み、リストのリストとして返す関数
(defun read-csv (filename)
  "Pythonのpd.read_csvに相当する基本機能"
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          collect (split-by-comma line))))

;; CSVファイルを読み込み、ヘッダーと本体に分けて返す関数
(defun read-csv-with-header (filename)
  "ヘッダー付きCSVファイルを読み込む"
  (let* ((all-data (read-csv filename))
         (header (first all-data))
         (body (rest all-data)))
    (values header body)))

;; 使用例
(defun demo-basic-csv ()
  (format t "CSV読み込みテスト:~%")
  (multiple-value-bind (header data) 
      (read-csv-with-header "height_weight.csv")
    (format t "ヘッダー: ~A~%" header)
    (format t "データ:~%")
    (dolist (row data)
      (format t "  ~A~%" row))))
```

このコードを`read_csv.lisp`として保存し、以下のコマンドで実行できます：

```bash
sbcl --script read_csv.lisp
```

## 数値への変換
CSVから読み込んだデータは文字列として扱われるため、数値計算のためには数値に変換する必要があります。Pythonの`to_numeric`関数に相当する機能を実装してみましょう：

```lisp
;; データを数値に変換する関数
(defun to-numeric (data &optional (columns '(1 2)))
  "指定された列のデータを数値に変換する（Pythonのto_numericに相当）"
  (mapcar (lambda (row)
            (loop for i from 0 below (length row)
                  for cell = (nth i row)
                  collect (if (member i columns)
                              (parse-integer cell :junk-allowed t)
                              cell)))
          data))

;; 使用例
(defun demo-numeric-conversion ()
  (format t "数値変換テスト:~%")
  (multiple-value-bind (header data) 
      (read-csv-with-header "height_weight.csv")
    (let ((numeric-data (to-numeric data)))
      (format t "変換後データ:~%")
      (dolist (row numeric-data)
        (format t "  ~A~%" row)))))
```

## データフレーム風の実装
Pythonのpandasの`DataFrame`に近い機能も実装してみましょう：

```lisp
;; データフレーム風の構造体を定義
(defstruct dataframe
  headers
  data)

;; データフレームを作成する関数
(defun make-df-from-csv (filename &optional numeric-columns)
  "CSVファイルからデータフレームを作成する（Pythonのpd.read_csvに近い機能）"
  (multiple-value-bind (headers data) (read-csv-with-header filename)
    (let ((processed-data (if numeric-columns 
                             (to-numeric data numeric-columns)
                             data)))
      (make-dataframe :headers headers :data processed-data))))

;; データフレームを表示する関数
(defun display-df (df &optional (n 5))
  "データフレームの先頭n行を表示する（Pythonのdf.head()に相当）"
  (format t "~%データフレーム:~%")
  (format t "ヘッダー: ~A~%" (dataframe-headers df))
  (format t "データ（先頭~A行）:~%" n)
  (loop for row in (subseq (dataframe-data df) 0 (min n (length (dataframe-data df))))
        for i from 1 to n
        do (format t "  ~A~%" row)))

;; 使用例
(defun demo-dataframe ()
  (format t "~%データフレーム風の使い方:~%")
  (let ((df (make-df-from-csv "height_weight.csv" '(1 2))))
    (display-df df)))
```

## 完全なコード例
上記のすべての機能を組み合わせた完全なコード例は以下のようになります：

```lisp
;;; read_csv.lisp - Pythonのpandas.read_csvに相当する機能の実装

;; 文字列をカンマで分割する関数
(defun split-by-comma (line)
  (loop for i = 0 then (1+ j)
        for j = (position #\, line :start i)
        collect (subseq line i j)
        until (null j)))

;; CSVファイルを読み込み、リストのリストとして返す関数
(defun read-csv (filename)
  "Pythonのpd.read_csvに相当する基本機能"
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          collect (split-by-comma line))))

;; CSVファイルを読み込み、ヘッダーと本体に分けて返す関数
(defun read-csv-with-header (filename)
  "ヘッダー付きCSVファイルを読み込む"
  (let* ((all-data (read-csv filename))
         (header (first all-data))
         (body (rest all-data)))
    (values header body)))

;; データを数値に変換する関数
(defun to-numeric (data &optional (columns '(1 2)))
  "指定された列のデータを数値に変換する（Pythonのto_numericに相当）"
  (mapcar (lambda (row)
            (loop for i from 0 below (length row)
                  for cell = (nth i row)
                  collect (if (member i columns)
                              (parse-integer cell :junk-allowed t)
                              cell)))
          data))

;; データフレーム風の構造体を定義
(defstruct dataframe
  headers
  data)

;; データフレームを作成する関数
(defun make-df-from-csv (filename &optional numeric-columns)
  "CSVファイルからデータフレームを作成する（Pythonのpd.read_csvに近い機能）"
  (multiple-value-bind (headers data) (read-csv-with-header filename)
    (let ((processed-data (if numeric-columns 
                             (to-numeric data numeric-columns)
                             data)))
      (make-dataframe :headers headers :data processed-data))))

;; データフレームを表示する関数
(defun display-df (df &optional (n 5))
  "データフレームの先頭n行を表示する（Pythonのdf.head()に相当）"
  (format t "~%データフレーム:~%")
  (format t "ヘッダー: ~A~%" (dataframe-headers df))
  (format t "データ（先頭~A行）:~%" n)
  (loop for row in (subseq (dataframe-data df) 0 (min n (length (dataframe-data df))))
        for i from 1 to n
        do (format t "  ~A~%" row)))

;; 使用例
(defun demo-read-csv ()
  (format t "~%CSVファイル読み込みデモ:~%")
  
  ;; 基本的な読み込み
  (format t "1. 基本的な読み込み:~%")
  (multiple-value-bind (header data) 
      (read-csv-with-header "height_weight.csv")
    (format t "ヘッダー: ~A~%" header)
    (format t "データ（生）:~%")
    (dolist (row data)
      (format t "  ~A~%" row)))
  
  ;; 数値変換
  (format t "~%2. 数値変換:~%")
  (multiple-value-bind (header data) 
      (read-csv-with-header "height_weight.csv")
    (let ((numeric-data (to-numeric data)))
      (format t "データ（数値変換後）:~%")
      (dolist (row numeric-data)
        (format t "  ~A~%" row))))
  
  ;; データフレーム風の使い方
  (format t "~%3. データフレーム風の使い方:~%")
  (let ((df (make-df-from-csv "height_weight.csv" '(1 2))))
    (display-df df)))

;; メイン関数
(defun main ()
  (format t "~%Lispデータ分析 - Pythonライクなread_csv実装~%")
  (demo-read-csv))

;; プログラム実行
(main)
```

## Pythonとの比較
Pythonでは同様の処理が以下のように簡潔に書けます：

```python
import pandas as pd

# CSVファイルを読み込む
df = pd.read_csv('height_weight.csv')

# データの確認
print("データの先頭:")
print(df.head())

# 基本情報
print("\nデータの基本情報:")
print(df.info())
```

Lispでは、Pythonの`pandas`のような高機能なライブラリがないため、基本的な機能から自分で実装する必要がありますが、その過程でLispの関数型プログラミングの特徴を活かした実装ができました。特に、Lispの`loop`マクロや`mapcar`関数などを使うことで、データ処理のコードを簡潔に書くことができます。

# おわりに
これで基本的なCSVファイルの読み込み機能が実装できました。
Pythonでは`df.mean()`、`df.std()`などの簡単な関数呼び出しで済む処理を、Lispでは一から実装する必要がありますが、Pythonでの経験を活かしながら、Lispの特徴を理解するよい機会になると思います。Lispの関数型プログラミングの特徴を活かした実装方法を探っていきましょう。

## 次回予告
次回は、このデータを使って基本統計量（平均、中央値、分散、標準偏差）を計算する関数を実装していきます。