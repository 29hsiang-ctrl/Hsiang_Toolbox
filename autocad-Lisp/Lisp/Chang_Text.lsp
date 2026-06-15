;;; -----------------------------------------------------------
;;; 指令: CT (Chang_Text)
;;; 功能: 框選範圍內的文字，統一修改「內容」 (已移除修改型式功能)
;;; -----------------------------------------------------------

(defun c:CT (/ ss newContent i ent entData oldEcho)
  (setq oldEcho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (princ "\n汪汪！執行 CT (快速修改內容)...")

  ;; 1. 框選文字
  (princ "\n請框選要修改的文字物件: ")
  (setq ss (ssget '((0 . "*TEXT"))))

  (if ss
    (progn
      ;; 2. 只詢問新內容 (移除了詢問型式的步驟)
      (setq newContent (getstring T "\n輸入新內容 (不改請按Enter): "))

      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (setq entData (entget ent))
        
        ;; 3. 僅執行內容修改 (群組碼 1)
        (if (/= newContent "")
          (setq entData (subst (cons 1 newContent) (assoc 1 entData) entData))
        )
        
        ;; (已移除修改型式的程式碼)

        (entmod entData)
        (entupd ent)
        (setq i (1+ i))
      )
      (princ (strcat "\n完成！修改了 " (itoa (sslength ss)) " 個物件。"))
    )
    (princ "\n未選取文字。")
  )
  
  (setvar "CMDECHO" oldEcho)
  
  ;; 載入回報
  (princ "\n汪汪！CT (修改文字) 已載入！")
  (princ)
)