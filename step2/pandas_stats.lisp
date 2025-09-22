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