;;; -----------------------------------------------------------
;;; 指令: SLT (Slot)
;;; 功能: 繪製長圓孔 (自動建立圖塊版)
;;; 特性: 
;;; 1. 自動建立圖塊，名稱範例: "10x30-長圓孔"
;;; 2. 插入點設為中心點 (十字交叉處)
;;; 3. 若圖塊已存在，直接插入；若不存在，自動繪製並定義
;;; 4. 內含: 青色外框 + 紅色中心線 + 紅色填充
;;; -----------------------------------------------------------

(defun c:SLT (/ diam totalLen blkName pt ss oldEcho oldLayer slotObj cLineH cLineV hatchObj r halfLen centerDist ang p1 p2 p3 p4 c1 c2 extDist cLineStart cLineEnd vLineStart vLineEnd)
  (vl-load-com)
  (setq oldEcho (getvar "CMDECHO"))
  (setq oldLayer (getvar "CLAYER"))
  (setvar "CMDECHO" 0)
  (princ "\n汪汪！執行 SLT (長圓孔圖塊版)...")

  ;; --- 0. 自動準備環境 ---
  (Hsiang_PrepareLayers)

  ;; --- 1. 獲取參數 ---
  (if (null *Hsiang_Slot_Diam*) (setq *Hsiang_Slot_Diam* 10.0))
  (setq diam (getdist (strcat "\n請輸入槽直徑 (寬度) <" (rtos *Hsiang_Slot_Diam* 2 2) ">: ")))
  (if (null diam) (setq diam *Hsiang_Slot_Diam*) (setq *Hsiang_Slot_Diam* diam))

  (if (null *Hsiang_Slot_TotalLen*) (setq *Hsiang_Slot_TotalLen* 30.0))
  (setq totalLen (getdist (strcat "\n請輸入總長度 (最外側距離) <" (rtos *Hsiang_Slot_TotalLen* 2 2) ">: ")))
  (if (null totalLen) (setq totalLen *Hsiang_Slot_TotalLen*) (setq *Hsiang_Slot_TotalLen* totalLen))

  ;; 防呆
  (if (< totalLen diam)
    (alert "錯誤：總長度不能小於直徑！")
    (progn
      
      ;; --- 2. 產生圖塊名稱 (例如 10x30-長圓孔) ---
      (setq blkName (strcat (Hsiang_FormatNum diam) "x" (Hsiang_FormatNum totalLen) "-長圓孔"))
      
      ;; 詢問插入點
      (setq pt (getpoint "\n指定插入點 (中心): "))

      (if pt
        (progn
          ;; --- 3. 判斷圖塊是否存在 ---
          (if (tblsearch "BLOCK" blkName)
            ;; A. 如果圖塊已存在 -> 直接插入
            (progn
              (princ (strcat "\n圖塊 [" blkName "] 已存在，直接插入..."))
              (command "_.-INSERT" blkName pt 1 1 0)
            )
            
            ;; B. 如果圖塊不存在 -> 繪製 -> 定義成塊 -> 插入
            (progn
              (princ (strcat "\n建立新圖塊 [" blkName "]..."))
              (setq ss (ssadd)) ; 建立選擇集容器

              ;; 計算幾何 (暫時畫在 pt 上，等一下包成塊)
              (setq centerDist (- totalLen diam))
              (setq r (/ diam 2.0))
              (setq halfLen (/ centerDist 2.0))
              (setq ang 0.0) ; 強制水平

              ;; 左右圓心
              (setq c1 (polar pt (+ ang pi) halfLen))
              (setq c2 (polar pt ang halfLen))

              ;; 四個頂點
              (setq p1 (polar c2 (- ang (/ pi 2)) r))
              (setq p2 (polar c2 (+ ang (/ pi 2)) r))
              (setq p3 (polar c1 (+ ang (/ pi 2)) r))
              (setq p4 (polar c1 (- ang (/ pi 2)) r))

              ;; --- 繪製實體並加入選擇集 (SS) ---
              
              ;; 1. 外框 (青色)
              (entmake (list 
                         '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
                         '(8 . "SLOT_OUTLINE") '(62 . 4) '(90 . 4) '(70 . 1)
                         (cons 10 p1) '(42 . 1.0)
                         (cons 10 p2) '(42 . 0.0)
                         (cons 10 p3) '(42 . 1.0)
                         (cons 10 p4) '(42 . 0.0)
              ))
              (setq slotObj (entlast))
              (ssadd slotObj ss)

              ;; 2. 中心線 (紅色) - 固定凸出 2.0
              (setq extDist 2.0)
              
              ;; 水平
              (setq cLineStart (polar pt (+ ang pi) (+ halfLen r extDist)))
              (setq cLineEnd   (polar pt ang (+ halfLen r extDist)))
              (entmake (list '(0 . "LINE") '(8 . "CENTER") '(62 . 1) (cons 10 cLineStart) (cons 11 cLineEnd)))
              (ssadd (entlast) ss)

              ;; 垂直
              (setq vLineStart (polar pt (+ ang (/ pi 2)) (+ r extDist)))
              (setq vLineEnd   (polar pt (- ang (/ pi 2)) (+ r extDist)))
              (entmake (list '(0 . "LINE") '(8 . "CENTER") '(62 . 1) (cons 10 vLineStart) (cons 11 vLineEnd)))
              (ssadd (entlast) ss)

              ;; 3. 填充 (紅色) - 比例固定 10
              (setvar "CLAYER" "HATCH")
              (command "_.-HATCH" "_P" "ANSI31" 10 0 "_S" slotObj "" "")
              (ssadd (entlast) ss) ; 抓取剛剛的填充線
              (setvar "CLAYER" oldLayer)

              ;; --- 製作圖塊 ---
              ;; 指令: BLOCK 名稱 基準點 選取物件 ""
              (command "_.-BLOCK" blkName pt ss "")
              
              ;; --- 插入圖塊 ---
              ;; 因為 BLOCK 指令會把剛剛畫的物件「吃掉」(轉成定義)，所以要在原地 insert 回來
              (command "_.-INSERT" blkName pt 1 1 0)
              
              (princ (strcat "\n成功！已建立並插入圖塊: " blkName))
            )
          )
        )
        (princ "\n取消操作。")
      )
    )
  )

  (setvar "CMDECHO" oldEcho)
  (princ "\n汪汪！SLT (自動圖塊版) 已載入！")
  (princ)
)

;;; --- 輔助函式：數字轉字串 (整數不留小數點) ---
(defun Hsiang_FormatNum (val)
  (if (= (rem val 1.0) 0.0)
    (itoa (fix val))     ; 如果是整數 (例 10.0) -> "10"
    (rtos val 2 1)       ; 如果是小數 (例 10.5) -> "10.5"
  )
)

;;; --- 輔助函式：準備圖層 ---
(defun Hsiang_PrepareLayers ( / )
  (if (null (tblsearch "LAYER" "CENTER"))
    (entmake (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord") '(100 . "AcDbLayerTableRecord") '(2 . "CENTER") '(70 . 0) '(62 . 1) '(6 . "Continuous"))))
  (if (null (tblsearch "LAYER" "HATCH"))
    (entmake (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord") '(100 . "AcDbLayerTableRecord") '(2 . "HATCH") '(70 . 0) '(62 . 1))))
  (if (null (tblsearch "LAYER" "SLOT_OUTLINE"))
    (entmake (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord") '(100 . "AcDbLayerTableRecord") '(2 . "SLOT_OUTLINE") '(70 . 0) '(62 . 4))))
)