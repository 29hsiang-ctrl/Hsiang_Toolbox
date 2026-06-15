;;; -----------------------------------------------------------
;;; 指令: T (Thickness)
;;; 功能: 將聚合線偏移指定距離，並自動封閉頭尾 (生成厚度)
;;; -----------------------------------------------------------

(defun c:T (/ dist ent entObj pt p1s p1e p2s p2e line1 line2 entOffset ssJoin oldEcho)
  (vl-load-com)
  (setq oldEcho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (setvar "PEDITACCEPT" 1)
  (princ "\n汪汪！執行 T (聚合線生成厚度)...")

  ;; 1. 獲取偏移距離 (全域變數記憶功能)
  (if (null *Hsiang_Thick_Dist*) (setq *Hsiang_Thick_Dist* 2.5))
  (setq dist (getdist (strcat "\n請輸入厚度 <" (rtos *Hsiang_Thick_Dist* 2 2) ">: ")))
  (if (null dist) (setq dist *Hsiang_Thick_Dist*) (setq *Hsiang_Thick_Dist* dist))

  ;; 2. 選取物件
  (while (setq ent (car (entsel "\n請選取聚合線 (Polyline): ")))
    (setq entObj (vlax-ename->vla-object ent))
    
    (if (wcmatch (cdr (assoc 0 (entget ent))) "*POLYLINE")
      (progn
        ;; 3. 指定方向並偏移
        (setq pt (getpoint "\n請點選生成方向的一點: "))
        
        (if pt
          (progn
            ;; 執行偏移
            (command "_.OFFSET" dist ent pt "")
            (setq entOffset (entlast))
            
            ;; 4. 抓取端點
            (setq p1s (vlax-curve-getStartPoint ent))
            (setq p1e (vlax-curve-getEndPoint ent))
            (setq p2s (vlax-curve-getStartPoint entOffset))
            (setq p2e (vlax-curve-getEndPoint entOffset))
            
            ;; 5. 畫封閉線 (判斷最近距離)
            (if (< (distance p1s p2s) (distance p1s p2e))
              (progn
                (command "_.LINE" p1s p2s "") (setq line1 (entlast))
                (command "_.LINE" p1e p2e "") (setq line2 (entlast))
              )
              (progn
                (command "_.LINE" p1s p2e "") (setq line1 (entlast))
                (command "_.LINE" p1e p2s "") (setq line2 (entlast))
              )
            )

            ;; 6. 結合
            (command "_.PEDIT" ent "_J" entOffset line1 line2 "" "")
            
            (princ "\n成功！已生成厚度。")
          )
          (princ "\n取消操作。")
        )
      )
      (princ "\n[錯誤] 這不是聚合線喔！")
    )
    (princ "\n繼續選取 (按 Esc 結束)...")
  )

  (setvar "CMDECHO" oldEcho)
  
  ;; 載入回報
  (princ "\n汪汪！T (生成厚度) 已載入！")
  (princ)
)