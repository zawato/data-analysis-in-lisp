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
