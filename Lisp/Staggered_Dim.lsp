;;; -----------------------------------------------------------
;;; 指令: SCD (Staggered Continuous Dimension)
;;; 功能: 交錯連續標註 (升級完美防呆版)
;;; 特色: 
;;; 1. 完美支援原生滑鼠拉動預覽 (第一與第二個)
;;; 2. 徹底防禦 OSNAP (物件鎖點) 干擾
;;; 3. 精準計算 UCS (使用者座標系統) 支援任意旋轉視角
;;; -----------------------------------------------------------

(defun c:SCD (/ pt1 pt2 lastEnt1 dim1 ent1 ucsAngle dimAngle angDiff isHoriz loc1 pt3 lastEnt2 dim2 ent2 loc2 prevPt curPt count refLoc oldEcho)
  (vl-load-com)
  (setq oldEcho (getvar "CMDECHO"))
  
  ;; 打開 CMDECHO 讓使用者能看到 AutoCAD 原生的標註提示與預覽
  (setvar "CMDECHO" 1)
  (princ "\n汪汪！執行 SCD (交錯連續標註 - 完美升級版)...")

  ;; --- 1. 畫第一層 (單數) ---
  (setq pt1 (getpoint "\n指定第一條延伸線原點: "))
  (if pt1
    (progn
      (setq pt2 (getpoint pt1 "\n指定第二條延伸線原點: "))
      (if pt2
        (progn
          (princ "\n汪汪！請拉動滑鼠，決定「第一層 (單數)」的高度並點擊...")
          (setq lastEnt1 (entlast))
          
          ;; 呼叫原生指令，並加上 _non 防止原始點被二次鎖點干擾
          (command "_.DIMLINEAR" "_non" pt1 "_non" pt2)
          ;; 暫停程式，讓使用者享受原生拉動標註的快感
          (while (> (getvar "CMDACTIVE") 0) (command pause))
          
          (setq dim1 (entlast))
          
          ;; 檢查使用者是否有按 ESC 取消
          (if (not (equal lastEnt1 dim1))
            (progn
              (setq ent1 (entget dim1))
              
              ;; --- 核心數學：判斷標註方向 (完美計算 UCS 與 WCS 角度差) ---
              (setq ucsAngle (angle '(0 0 0) (trans '(1 0 0) 1 0)))
              (setq dimAngle (cdr (assoc 50 ent1))) ; 取得標註的真實旋轉角
              (setq angDiff (rem (abs (- dimAngle ucsAngle)) pi))
              ;; 若角度差接近 0 或 180 度，代表是水平標註
              (setq isHoriz (or (< angDiff 0.01) (> angDiff (- pi 0.01))))
              
              ;; 取得標註線位置並轉換為 UCS (極度重要，否則座標會亂飛)
              (setq loc1 (trans (cdr (assoc 10 ent1)) 0 1))

              ;; --- 2. 畫第二層 (偶數) ---
              (setq pt3 (getpoint pt2 "\n指定下一條延伸線原點 (Enter 結束): "))
              (if pt3
                (progn
                  (princ "\n汪汪！請拉動滑鼠，決定「第二層 (偶數)」的高度並點擊...")
                  (setq lastEnt2 (entlast))
                  (command "_.DIMLINEAR" "_non" pt2 "_non" pt3)
                  (while (> (getvar "CMDACTIVE") 0) (command pause))
                  
                  (setq dim2 (entlast))
                  
                  (if (not (equal lastEnt2 dim2))
                    (progn
                      (setq ent2 (entget dim2))
                      (setq loc2 (trans (cdr (assoc 10 ent2)) 0 1))

                      ;; --- 3. 開始極速自動化迴圈 ---
                      (setvar "CMDECHO" 0) ; 關閉提示，開始無腦連點
                      (setq prevPt pt3)
                      (setq count 3) ; 接下來準備畫第 3 個 (單數層)

                      (while (setq curPt (getpoint prevPt "\n指定下一條延伸線原點 (Enter 結束): "))
                        
                        ;; 決定要參考哪一層的座標高度
                        (if (= (rem count 2) 1)
                          (setq refLoc loc1)
                          (setq refLoc loc2)
                        )

                        ;; 強制使用 _H (水平) 或 _V (垂直) 確保方向不變，並使用 _non 防止鎖點干擾
                        (if isHoriz
                          (command "_.DIMLINEAR" "_non" prevPt "_non" curPt "_H" "_non" refLoc)
                          (command "_.DIMLINEAR" "_non" prevPt "_non" curPt "_V" "_non" refLoc)
                        )

                        (setq prevPt curPt)
                        (setq count (1+ count))
                      )
                    )
                    (princ "\n取消了第二層標註。")
                  )
                )
              )
            )
            (princ "\n取消了第一層標註。")
          )
        )
      )
    )
  )

  (setvar "CMDECHO" oldEcho)
  (princ "\n汪汪！SCD (交錯標註) 完美收工！")
  (princ)
)