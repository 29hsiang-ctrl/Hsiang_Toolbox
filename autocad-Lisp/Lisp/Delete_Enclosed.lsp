;;; -----------------------------------------------------------
;;; 指令: DE (Delete Enclosed) - 隱形圍籬切割版
;;; 功能: 
;;; 1. 建立「內部隱形圍籬」(Offset)
;;; 2. 使用 TRIM + Fence 指令強制切斷跨越線
;;; 3. 使用 WP (Window Polygon) 刪除完全內部物件
;;; -----------------------------------------------------------

(defun c:DE (/ Hsiang_Ent Hsiang_Pt Hsiang_Obj Hsiang_ClosePt Hsiang_Vec Hsiang_Len Hsiang_UnitVec Hsiang_OffsetPt Hsiang_FenceEnt Hsiang_FencePts Hsiang_PtList Hsiang_SS Hsiang_N Hsiang_DelEnt Hsiang_OldEcho Hsiang_OldOsmode Hsiang_OffsetDist)
  (vl-load-com)
  (setq Hsiang_OldEcho (getvar "CMDECHO"))
  (setq Hsiang_OldOsmode (getvar "OSMODE"))
  (setvar "CMDECHO" 0)
  (setvar "OSMODE" 0) ; 關閉鎖點 (非常重要)
  (princ "\n汪汪！執行 DE (隱形圍籬切割)...")

  ;; --- 1. 選取邊界 ---
  (setq Hsiang_Ent (car (entsel "\n請選取「封閉聚合線」作為切割邊界: ")))

  (if Hsiang_Ent
    (progn
      ;; --- 2. 指定內部點 ---
      (setq Hsiang_Pt (getpoint "\n請在聚合線「內部」點一下 (指定要刪除的區域): "))
      
      (if Hsiang_Pt
        (progn
          (princ "\n[1/3] 正在準備切割工具...")
          
          (setq Hsiang_Obj (vlax-ename->vla-object Hsiang_Ent))
          (if (wcmatch (vla-get-ObjectName Hsiang_Obj) "AcDbPolyline,AcDb2dPolyline")
            (progn
              ;; --- 縮放視窗 (確保指令運作正常) ---
              (command "_.ZOOM" "_O" Hsiang_Ent "")
              
              ;; --- 3. 建立「隱形圍籬」 (Offset Polyline) ---
              ;; 計算一個非常靠近邊緣的內部點
              (setq Hsiang_OffsetDist (* (getvar "VIEWSIZE") 0.005)) ; 偏移量為視窗高度的 0.5%
              (setq Hsiang_ClosePt (vlax-curve-getClosestPointTo Hsiang_Obj Hsiang_Pt))
              
              ;; 計算向量 (從最近點指向內部點)
              (setq Hsiang_Vec (mapcar '- Hsiang_Pt Hsiang_ClosePt))
              (setq Hsiang_Len (distance '(0 0 0) Hsiang_Vec))
              (if (zerop Hsiang_Len) (setq Hsiang_Len 1.0)) ; 防除以零
              
              ;; 計算偏移目標點 (往內部微移)
              (setq Hsiang_UnitVec (mapcar '/ Hsiang_Vec (list Hsiang_Len Hsiang_Len Hsiang_Len)))
              (setq Hsiang_OffsetPt (mapcar '+ Hsiang_ClosePt (mapcar '* Hsiang_UnitVec (list Hsiang_OffsetDist Hsiang_OffsetDist Hsiang_OffsetDist))))

              ;; 執行偏移指令 (Offset Through)
              ;; 這樣會產生一條「剛好在紅框裡面一點點」的線
              (command "_.OFFSET" "_Through" Hsiang_Ent Hsiang_OffsetPt "")
              (setq Hsiang_FenceEnt (entlast)) ; 抓取這條偏移線
              
              ;; --- 4. 執行修剪 (TRIM + Fence) ---
              (if (not (equal Hsiang_FenceEnt Hsiang_Ent)) ; 確保偏移成功
                (progn
                  (princ "\n[2/3] 正在切斷跨越邊界的線...")
                  
                  ;; 獲取偏移線的頂點
                  (setq Hsiang_FencePts (Hsiang_GetPolyVertices Hsiang_FenceEnt))
                  
                  ;; 呼叫 TRIM 指令
                  ;; 邏輯：以紅框(Ent)為邊界，用偏移線(FencePts)當作柵欄去碰觸要修剪的物件
                  (command "_.TRIM" Hsiang_Ent "" "_Fence")
                  (foreach pt Hsiang_FencePts (command pt))
                  (command "" "") ; 結束 TRIM
                  
                  ;; 刪除偏移線 (工具人功成身退)
                  (entdel Hsiang_FenceEnt)
                )
                (princ "\n[警告] 偏移失敗，嘗試直接刪除內部物件...")
              )

              ;; --- 5. 刪除內部物件 (WP) ---
              (princ "\n[3/3] 正在清空內部...")
              (setq Hsiang_PtList (Hsiang_GetPolyVertices Hsiang_Ent))
              
              (command "_.ZOOM" "0.95x") ; 縮小一點確保 WP 有效
              (setq Hsiang_SS (ssget "_WP" Hsiang_PtList))

              ;; 排除邊界本身
              (if Hsiang_SS (ssdel Hsiang_Ent Hsiang_SS))

              ;; 執行刪除
              (if (and Hsiang_SS (> (sslength Hsiang_SS) 0))
                (progn
                  (setq Hsiang_N 0)
                  (repeat (sslength Hsiang_SS)
                    (setq Hsiang_DelEnt (ssname Hsiang_SS Hsiang_N))
                    (entdel Hsiang_DelEnt)
                    (setq Hsiang_N (1+ Hsiang_N))
                  )
                  (princ (strcat "\n汪汪！成功切斷並清除了 " (itoa Hsiang_N) " 個物件。"))
                )
                (princ "\n範圍內已清空。")
              )
              
              (command "_.ZOOM" "_P")
            )
            (princ "\n[錯誤] 您選的不是聚合線。")
          )
        )
        (princ "\n未指定內部點。")
      )
    )
    (princ "\n未選取邊界。")
  )

  (setvar "CMDECHO" Hsiang_OldEcho)
  (setvar "OSMODE" Hsiang_OldOsmode)
  (princ)
)

;;; --- 輔助函式：獲取聚合線頂點 (支援 LWPOLYLINE) ---
(defun Hsiang_GetPolyVertices (ent / obj i endParam pt ptList)
  (setq obj (vlax-ename->vla-object ent))
  (setq endParam (fix (vlax-curve-getEndParam obj)))
  (setq i 0)
  (setq ptList '())
  (while (<= i endParam)
    (setq pt (vlax-curve-getPointAtParam obj i))
    (setq ptList (cons pt ptList))
    (setq i (1+ i))
  )
  (reverse ptList)
)