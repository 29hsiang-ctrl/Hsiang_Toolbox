;;; ============================================================
;;;  CountByPNUM.lsp  (v2 - 含動態距離參數統計)
;;;  依塊參照的 PNUM 屬性統計件數，並可加抓一個動態距離參數
;;;  指令: CNT
;;;
;;;  用法:
;;;    1. APPLOAD 載入本檔
;;;    2. 輸入 CNT
;;;    3. 程式先問：要統計哪個距離參數名稱?
;;;         - 直接 Enter = 不抓距離，只統計件數
;;;         - 輸入參數名 (例如 寬度) = 連該距離值一起統計
;;;    4. 框選要統計的範圍 (或直接 Enter 選全部)
;;;    5. 點一下放置統計表的位置
;;;
;;;  輸出: 件號 / 距離 / 數量
;;;    - 找不到該距離參數的塊，距離欄顯示 "—"，件數照算
;;;    - 同件號但距離不同者，會分列顯示
;;;
;;;  可調設定:
;;;    *TAG*   = 件號屬性標籤名 (預設 PNUM)
;;;    *TXTHT* = 表格文字高度
;;; ============================================================

(vl-load-com)

(setq *TAG*   "PNUM")
(setq *TXTHT* 2.5)

;; --- 讀指定標籤的屬性值 ---
(defun get-att (ent tag / obj val att)
  (setq val nil)
  (setq obj (vlax-ename->vla-object ent))
  (if (= (vla-get-hasattributes obj) :vlax-true)
    (foreach att (vlax-invoke obj 'getattributes)
      (if (= (strcase (vla-get-tagstring att)) (strcase tag))
        (setq val (vla-get-textstring att))
      )
    )
  )
  val
)

;; --- 讀指定名稱的動態參數值 (找不到回 nil) ---
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

;; --- 數字轉精簡字串 (去掉多餘小數) ---
(defun num->str (n)
  (if (numberp n)
    (if (= n (fix n))
      (itoa (fix n))
      (rtos n 2 1)
    )
    "—"
  )
)

;; --- 主指令 ---
(defun c:CNT ( / pname ss i ent pnum data key pair
                 pt total skipped report dval dstr tabpos)

  ;; 先問要抓哪個距離參數
  (setq pname (getstring T "\n要統計哪個距離參數? (直接 Enter = 只算件數): "))
  (if (= pname "") (setq pname nil))

  (princ "\n選取要統計的塊 (直接 Enter 選全部圖面)...")
  (setq ss (ssget '((0 . "INSERT"))))
  (if (null ss) (setq ss (ssget "_X" '((0 . "INSERT")))))

  (if (null ss)
    (progn (princ "\n找不到任何塊參照。") (princ))
    (progn
      (setq data '() i 0 total 0 skipped 0)

      (while (< i (sslength ss))
        (setq ent  (ssname ss i))
        (setq pnum (get-att ent *TAG*))
        (if (and pnum (/= pnum ""))
          (progn
            (if pname
              (setq dval (get-dynprop ent pname))
              (setq dval nil))
            (setq dstr (num->str dval))
            (setq key (strcat pnum "\t" dstr))
            (setq pair (assoc key data))
            (if pair
              (setq data (subst (cons key (1+ (cdr pair))) pair data))
              (setq data (cons (cons key 1) data))
            )
            (setq total (1+ total))
          )
          (setq skipped (1+ skipped))
        )
        (setq i (1+ i))
      )

      (if (null data)
        (princ (strcat "\n選取的塊都沒有 " *TAG* " 屬性，無法統計。"))
        (progn
          (setq data (vl-sort data
                       (function (lambda (a b) (< (car a) (car b))))))

          (if pname
            (setq report "件號        距離        數量")
            (setq report "件號        數量"))

          (foreach pair data
            (setq key  (car pair))
            (setq pnum (substr key 1 (vl-string-search "\t" key)))
            (setq dstr (substr key (+ 2 (vl-string-search "\t" key))))
            (if pname
              (setq report (strcat report "\\P" pnum "        "
                                   dstr "        " (itoa (cdr pair))))
              (setq report (strcat report "\\P" pnum "        "
                                   (itoa (cdr pair))))
            )
          )
          (setq report (strcat report "\\P-----------\\P總計            "
                               (itoa total)))

          (setq pt (getpoint "\n指定統計表放置點: "))
          (if pt
            (progn
              (entmake
                (list '(0 . "MTEXT")
                      '(100 . "AcDbEntity")
                      '(100 . "AcDbMText")
                      (cons 10 pt)
                      (cons 40 *TXTHT*)
                      (cons 1 report)
                      '(71 . 1)
                      (cons 41 (* *TXTHT* 16))))
              (princ (strcat "\n完成，共統計 " (itoa total) " 件。"))
            )
          )
        )
      )

      (if (> skipped 0)
        (princ (strcat "\n注意: " (itoa skipped)
                       " 個塊沒有 " *TAG* " 屬性，未列入。"))
      )
    )
  )
  (princ)
)

(princ "\nCountByPNUM v2 已載入。輸入 CNT 開始統計。")
(princ)
