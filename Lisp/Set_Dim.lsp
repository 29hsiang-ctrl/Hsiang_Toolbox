;;; -----------------------------------------------------------
;;; 指令: SD (Set DimStyle)
;;; 功能: 選取一個標註物件，將其「標註型式」設為目前使用中
;;; -----------------------------------------------------------

(defun c:SD (/ ent entData objType styleName oldEcho)
  (vl-load-com)
  (setq oldEcho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (princ "\n汪汪！準備設定目前標註型式...")

  ;; 1. 選取物件
  (setq ent (car (entsel "\n請選取目標標註 (要把誰設為目前?): ")))

  (if ent
    (progn
      (setq entData (entget ent))
      (setq objType (cdr (assoc 0 entData)))
      
      ;; 2. 檢查是否為標註物件
      (if (wcmatch objType "*DIMENSION*")
        (progn
          ;; 3. 獲取標註型式名稱 (群組碼 3)
          (setq styleName (cdr (assoc 3 entData)))
          
          ;; 4. 設定為目前型式 (使用指令 -DIMSTYLE -> Restore)
          (command "_.DIMSTYLE" "_Restore" styleName)
          
          (princ (strcat "\n\n汪汪！成功！\n目前標註型式已切換為: [" styleName "]"))
          (princ "\n現在畫出來的標註都會長這樣囉！")
        )
        (princ "\n\n[錯誤] 您選到的不是標註物件喔！(是 Line 或 Block 嗎?)")
      )
    )
    (princ "\n取消選取。")
  )

  (setvar "CMDECHO" oldEcho)
  
  ;; 載入回報
  (princ "\n汪汪！SD (設定目前標註型式) 已載入！")
  (princ)
)