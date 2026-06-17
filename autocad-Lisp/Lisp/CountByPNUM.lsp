;;; ============================================================
;;;  CountByPNUM.lsp  (v9 - TABLE 實體，自動對齊欄位)
;;;  指令: CNT
;;;
;;;  步驟:
;;;    [1] 距離參數名稱 (預設 距離1，- = 只算件數)
;;;    [2] 選取塊
;;;    [3] 若有可見性參數，詢問篩選條件 (否則自動跳過)
;;;    [4] 指定統計表放置點
;;;
;;;  輸出: AutoCAD TABLE 實體
;;;    提示                 件號   距離         數量
;;;    100*100*4t-不鏽鋼管  A      L= 1185 ~   1 隻
;;;                         B      L= 2400 ~   1 隻
;;;
;;;  可調欄寬倍率 (× *TXTHT*):
;;;    *CP*  提示欄寬   *CPnum* 件號欄寬
;;;    *CDS* 距離欄寬   *CQT*   數量欄寬
;;; ============================================================

(vl-load-com)

(setq *TAG*         "PNUM")
(setq *TXTHT*       2.5)
(setq *ROWHT*       2.0)   ; 行高倍率
(setq *VIS-DEFAULT* "可見性")
;; 各欄寬倍率 (× *TXTHT*)  |提示|件號|L=|距離|~|數量|隻|
(setq *WP*  14)   ; 提示
(setq *WN*   4)   ; 件號
(setq *WL*   3)   ; L=
(setq *WD*   6)   ; 距離值
(setq *WT*   2)   ; ~
(setq *WQ*   3)   ; 數量值
(setq *WU*   3)   ; 隻

;; --- 從塊定義找指定 tag 的 ATTDEF 提示文字 ---
(defun get-attdef-prompt (bname tag / be edata etype prm)
  (setq prm "")
  (setq be (tblobjname "BLOCK" bname))
  (if be
    (progn
      (setq be (entnext be))
      (while be
        (setq edata (entget be))
        (setq etype (cdr (assoc 0 edata)))
        (if (and (= etype "ATTDEF")
                 (= (strcase (cdr (assoc 2 edata))) (strcase tag)))
          (setq prm (cdr (assoc 3 edata)))
        )
        (setq be (entnext be))
      )
    )
  )
  prm
)

;; --- 讀屬性值與提示，回傳 (list val prm) ---
(defun get-att-both (ent tag / obj bname att val prm)
  (setq val nil prm "")
  (setq obj (vlax-ename->vla-object ent))
  (setq bname
    (if (vlax-property-available-p obj 'EffectiveName)
      (vla-get-effectivename obj)
      (cdr (assoc 2 (entget ent)))))
  (if (= (vla-get-hasattributes obj) :vlax-true)
    (foreach att (vlax-invoke obj 'getattributes)
      (if (= (strcase (vla-get-tagstring att)) (strcase tag))
        (setq val (vla-get-textstring att))
      )
    )
  )
  (if val (setq prm (get-attdef-prompt bname tag)))
  (list val prm)
)

;; --- 讀動態參數值 ---
(defun get-dynprop (ent pname / obj val prop)
  (setq val nil)
  (setq obj (vlax-ename->vla-object ent))
  (if (and (vlax-property-available-p obj 'IsDynamicBlock)
           (= (vla-get-isdynamicblock obj) :vlax-true))
    (foreach prop (vlax-invoke obj 'getdynamicblockproperties)
      (if (= (strcase (vla-get-propertyname prop)) (strcase pname))
        (setq val (vlax-get prop 'Value))
      )
    )
  )
  val
)

;; --- 檢查選取集內是否有塊含 *VIS-DEFAULT* 動態參數 ---
(defun cnt-any-vis-param (ss / i ent obj prop found)
  (setq i 0 found nil)
  (while (and (< i (sslength ss)) (not found))
    (setq ent (ssname ss i)
          obj (vlax-ename->vla-object ent))
    (if (and (vlax-property-available-p obj 'IsDynamicBlock)
             (= (vla-get-isdynamicblock obj) :vlax-true))
      (foreach prop (vlax-invoke obj 'getdynamicblockproperties)
        (if (= (strcase (vla-get-propertyname prop)) (strcase *VIS-DEFAULT*))
          (setq found T)
        )
      )
    )
    (setq i (1+ i))
  )
  found
)

;; --- 數字轉精簡字串 ---
(defun num->str (n)
  (if (numberp n)
    (if (= n (fix n)) (itoa (fix n)) (rtos n 2 1))
    "-"
  )
)

;; --- 從 tab 分隔字串取第 n 個欄位 (0-based) ---
(defun tab-field (str n / pos i start)
  (setq i 0 start 0)
  (while (and (< i n) (setq pos (vl-string-search "\t" str start)))
    (setq start (1+ pos) i (1+ i))
  )
  (if (< i n)
    ""
    (progn
      (setq pos (vl-string-search "\t" str start))
      (if pos (substr str (1+ start) (- pos start)) (substr str (1+ start)))
    )
  )
)

;; --- 建立 TABLE 實體 ---
;;  row 0 = 算料表 (合併標題)
;;  row 1 = 標頭   |提示|件號|L=|距離|~|數量|隻|
;;  row 2+= 資料
;;  last  = 總計
(defun cnt-make-table (pt data pname total /
    acad doc ms tbl numRows numCols rowHt r c row last-prm prm pnum dstr cnt)

  (setq acad    (vlax-get-acad-object)
        doc     (vla-get-activedocument acad)
        ms      (vla-get-modelspace doc)
        numRows (+ (length data) 3)   ; 標題 + 標頭 + 資料 + 總計
        numCols (if pname 7 4)
        rowHt   (* *TXTHT* *ROWHT*))

  ;; 建立表格 (row 0 為合併標題列，不隱藏)
  (setq tbl (vla-addtable ms
               (vlax-3d-point (car pt) (cadr pt) (if (caddr pt) (caddr pt) 0.0))
               numRows numCols rowHt (* *TXTHT* *WP*)))

  ;; 設定欄寬
  (if pname
    (progn
      (vla-setcolumnwidth tbl 0 (* *TXTHT* *WP*))
      (vla-setcolumnwidth tbl 1 (* *TXTHT* *WN*))
      (vla-setcolumnwidth tbl 2 (* *TXTHT* *WL*))
      (vla-setcolumnwidth tbl 3 (* *TXTHT* *WD*))
      (vla-setcolumnwidth tbl 4 (* *TXTHT* *WT*))
      (vla-setcolumnwidth tbl 5 (* *TXTHT* *WQ*))
      (vla-setcolumnwidth tbl 6 (* *TXTHT* *WU*)))
    (progn
      (vla-setcolumnwidth tbl 0 (* *TXTHT* *WP*))
      (vla-setcolumnwidth tbl 1 (* *TXTHT* *WN*))
      (vla-setcolumnwidth tbl 2 (* *TXTHT* *WQ*))
      (vla-setcolumnwidth tbl 3 (* *TXTHT* *WU*))))

  ;; 設定各格文字高度
  (setq r 0)
  (while (< r numRows)
    (setq c 0)
    (while (< c numCols)
      (vl-catch-all-apply 'vla-setcelltextheight (list tbl r c *TXTHT*))
      (setq c (1+ c)))
    (setq r (1+ r)))

  ;; row 0: 合併標題「算料表」
  (vla-settext tbl 0 0 "算料表")

  ;; row 1: 欄位標頭
  (vla-settext tbl 1 0 "提示")
  (vla-settext tbl 1 1 "件號")
  (if pname
    (progn
      (vla-settext tbl 1 2 "L=")
      (vla-settext tbl 1 3 "距離")
      (vla-settext tbl 1 4 "~")
      (vla-settext tbl 1 5 "數量")
      (vla-settext tbl 1 6 "隻"))
    (progn
      (vla-settext tbl 1 2 "數量")
      (vla-settext tbl 1 3 "隻")))

  ;; row 2+: 資料列
  (setq row 2 last-prm nil)
  (foreach pair data
    (setq prm  (tab-field (car pair) 0)
          pnum (tab-field (car pair) 1)
          dstr (tab-field (car pair) 2)
          cnt  (cdr pair))
    (if (not (equal prm last-prm))
      (vla-settext tbl row 0 prm))
    (setq last-prm prm)
    (vla-settext tbl row 1 pnum)
    (if pname
      (progn
        (vla-settext tbl row 2 "L=")
        (vla-settext tbl row 3 dstr)
        (vla-settext tbl row 4 "~")
        (vla-settext tbl row 5 (itoa cnt))
        (vla-settext tbl row 6 "隻"))
      (progn
        (vla-settext tbl row 2 (itoa cnt))
        (vla-settext tbl row 3 "隻")))
    (setq row (1+ row)))

  ;; 總計列
  (vla-settext tbl row 0 "總計")
  (if pname
    (progn
      (vla-settext tbl row 5 (itoa total))
      (vla-settext tbl row 6 "隻"))
    (progn
      (vla-settext tbl row 2 (itoa total))
      (vla-settext tbl row 3 "隻")))

  tbl
)

;; --- 主指令 ---
(defun c:CNT ( / pname vis-pname vis-state vis-val ss i ent result pnum prm
                 data key pair pt total skipped dval dstr ents)

  ;; [1] 距離參數
  (setq pname (getstring T "\n[1] 距離參數名稱? (- = 只算件數) <距離1>: "))
  (cond
    ((= pname "")  (setq pname "距離1"))
    ((= pname "-") (setq pname nil))
  )

  ;; [2] 選取塊
  (princ "\n[2] 選取要統計的塊 (直接 Enter 選全部圖面)...")
  (setq ss (ssget '((0 . "INSERT"))))
  (if (null ss) (setq ss (ssget "_X" '((0 . "INSERT")))))

  (if (null ss)
    (progn (princ "\n找不到任何塊參照。") (princ))
    (progn
      (setq vis-pname nil vis-state nil)

      ;; [3] 可見性篩選
      (if (cnt-any-vis-param ss)
        (progn
          (setq vis-pname (getstring T (strcat "\n[3] 可見性參數名稱? (- = 不篩選) <" *VIS-DEFAULT* ">: ")))
          (cond
            ((= vis-pname "")  (setq vis-pname *VIS-DEFAULT*))
            ((= vis-pname "-") (setq vis-pname nil))
          )
          (if vis-pname
            (progn
              (setq vis-state (getstring T "\n    篩選狀態 (例: 平面 / 立面 / 剖刀): "))
              (if (= vis-state "") (setq vis-pname nil vis-state nil))
            )
          )
        )
        (princ "\n[3] 無可見性參數，自動跳過篩選。")
      )

      ;; 統計
      (setq data '() i 0 total 0 skipped 0)
      (while (< i (sslength ss))
        (setq ent    (ssname ss i))
        (setq result (get-att-both ent *TAG*))
        (setq pnum   (car result))
        (setq prm    (if (cadr result) (cadr result) ""))
        (if (and vis-pname
                 (progn
                   (setq vis-val (get-dynprop ent vis-pname))
                   (not (and vis-val (= (strcase vis-val) (strcase vis-state))))))
          (setq skipped (1+ skipped))
          (if (and pnum (/= pnum ""))
            (progn
              (setq dval (if pname (get-dynprop ent pname) nil))
              (setq dstr (num->str dval))
              (setq key (strcat prm "\t" pnum "\t" dstr))
              (setq pair (assoc key data))
              (if pair
                (setq data (subst (cons key (1+ (cdr pair))) pair data))
                (setq data (cons (cons key 1) data))
              )
              (setq total (1+ total))
            )
            (setq skipped (1+ skipped))
          )
        )
        (setq i (1+ i))
      )

      (if (null data)
        (princ "\n沒有符合條件的塊。")
        (progn
          (setq data (vl-sort data (function (lambda (a b) (< (car a) (car b))))))

          (setq pt (getpoint "\n[4] 指定統計表放置點: "))
          (if pt
            (progn
              (cnt-make-table pt data pname total)
              (princ (strcat "\n完成，共統計 " (itoa total) " 件。"))
              (if vis-pname
                (princ (strcat " [篩選: " vis-state "]"))
              )
            )
          )
        )
      )

      (if (> skipped 0)
        (princ (strcat "\n注意: " (itoa skipped)
                       " 個塊已略過（不符篩選或無 " *TAG* " 屬性）。"))
      )
    )
  )
  (princ)
)

(princ "\nCountByPNUM v11 已載入。輸入 CNT 開始統計。")
(princ)
