;;; -----------------------------------------------------------
;;; 指令: LL (Label)
;;; 功能: 自動編號 (MText 動態寬度 + 自訂高度 + 支援空格)
;;; 特性: 
;;; 1. 輸入內容時可按空白鍵 (必須按 Enter 確認)
;;; 2. 框框寬度隨文字長度自動調整 (留白係數 1.3)
;;; 3. 可輸入文字高度
;;; -----------------------------------------------------------

(defun c:LL (/ prefix startStr validChars currentIdx pt suffix finalTxt txtHeight)
  (vl-load-com)
  (setvar "CMDECHO" 0)
  (princ "\n汪汪！執行 Label (支援空格版)...")

  ;; --- 0. 自動準備環境 ---
  (Hsiang_PrepareEnvironment)

  ;; --- 1. 定義字元 (無 I, O) ---
  (setq validChars '("A" "B" "C" "D" "E" "F" "G" "H" "J" "K" "L" "M" "N" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"))

  ;; --- 2. 詢問設定 ---
  ;; A. 前綴 (修改：加入 T，允許輸入空格)
  (setq prefix (getstring T "\n請輸入固定前綴內容 (例如 W-3): "))
  (if (= prefix "") (setq prefix "W-3"))

  ;; B. 起始字母 (修改：加入 T，雖通常無空格但保持操作一致)
  (setq startStr (getstring T "\n請輸入起始字母 (預設 A, 按 Enter 跳過): "))
  (if (= startStr "")
    (setq currentIdx 0)
    (setq currentIdx (Hsiang_StrToIndex startStr validChars))
  )

  ;; C. 詢問文字高度
  (setq txtHeight (getdist "\n請輸入文字高度 <預設50>: "))
  (if (null txtHeight) (setq txtHeight 50.0)) 

  ;; --- 3. 循環放置 ---
  (while (setq pt (getpoint (strcat "\n指定放置點 [目前: " prefix (Hsiang_GetSuffix currentIdx validChars) "] <Esc結束>: ")))
    
    (setq suffix (Hsiang_GetSuffix currentIdx validChars))
    (setq finalTxt (strcat prefix suffix))

    ;; 建立 MTEXT
    (entmake (list 
               '(0 . "MTEXT")
               '(100 . "AcDbEntity")
               '(8 . "00編號")          ; 圖層
               '(100 . "AcDbMText")
               (cons 10 pt)             ; 插入點
               (cons 40 txtHeight)      ; 文字高度
               
               '(71 . 5)                ; 對正: 正中
               (cons 1 finalTxt)        ; 內容
               (cons 7 "CHINA")         ; 字型
               '(50 . 0.0)              ; 旋轉 0
               
               ;; 背景遮罩與邊框
               '(90 . 19)               ; 開啟背景 + 邊框
               '(63 . 256)              ; 背景色 ByLayer
               
               ;; 邊界係數 (留白)
               '(45 . 1.3)              
    ))
    
    (setq currentIdx (1+ currentIdx))
  )
  
  (setvar "CMDECHO" 1)
  (princ)
)

;;; --- 環境準備 ---
(defun Hsiang_PrepareEnvironment ( / )
  (if (null (tblsearch "LAYER" "00編號"))
    (entmake (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord") '(100 . "AcDbLayerTableRecord") '(2 . "00編號") '(70 . 0) '(62 . 1))))
  (if (null (tblsearch "STYLE" "CHINA"))
    (entmake (list '(0 . "STYLE") '(100 . "AcDbSymbolTableRecord") '(100 . "AcDbTextStyleTableRecord") '(2 . "CHINA") '(70 . 0) '(40 . 0.0) '(3 . "arial.ttf") '(4 . ""))))
)

;;; --- 演算法區域 ---
(defun Hsiang_GetSuffix (idx chars / len q r char1 char2)
  (setq len (length chars)) 
  (if (< idx len) (nth idx chars)
    (progn (setq q (/ idx len)) (setq r (rem idx len)) (setq char1 (nth (1- q) chars)) (setq char2 (nth r chars)) (if (and char1 char2) (strcat char1 char2) "Err"))))

(defun Hsiang_StrToIndex (str chars / idx c1 c2 idx1 idx2 len)
  (setq str (strcase str)) (setq len (length chars))
  (cond ((= (strlen str) 1) (if (setq idx (vl-position str chars)) idx 0))
        ((= (strlen str) 2) (setq c1 (substr str 1 1)) (setq c2 (substr str 2 1)) (setq idx1 (vl-position c1 chars)) (setq idx2 (vl-position c2 chars)) (if (and idx1 idx2) (+ (* (+ idx1 1) len) idx2) 0))
        (T 0)))

(princ "\n汪汪！LL (支援空格版) 已載入！")
(princ)