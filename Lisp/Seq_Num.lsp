;;; -----------------------------------------------------------
;;; 指令: NN (Sequential Number)
;;; 功能: 點選現有文字，依序將內容修改為遞增數字
;;; -----------------------------------------------------------

(defun c:NN (/ startNum sel ent entData objType oldEcho currentStr loopActive err)
  (vl-load-com)
  (setq oldEcho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (princ "\n汪汪！執行 NN (連續跳號修改)...")

  ;; 1. 詢問起始數字
  (setq startNum (getint "\n請輸入起始數字 <1>: "))
  (if (null startNum) (setq startNum 1)) ; 如果直接按 Enter，預設為 1

  (princ "\n請依序點選文字物件 (按 Enter 或 空白鍵 結束):")

  ;; 2. 迴圈選取 (修復空白點選終止問題 & R重複機制)
  (setq loopActive T)
  (while loopActive
    (setvar "ERRNO" 0) ; 每次選取前先重置錯誤碼
    
    ;; 設定 R 為可接受的關鍵字 (大小寫 r 均可)
    (initget "R")      
    (setq sel (entsel (strcat "\n點選文字變更為 [" (itoa startNum) "] (按 Enter 結束 / 輸入 R 重複上一數字): ")))
    
    ;; 取得選取後的錯誤碼狀態
    (setq err (getvar "ERRNO"))

    (cond
      ;; 狀況 A: 使用者輸入了 R 或 r
      ((= sel "R")
       (setq startNum (1- startNum))
       (princ (strcat "\n汪汪！退回上一個數字，下一次點選將變更為 [" (itoa startNum) "]"))
      )

      ;; 狀況 B: 成功選到物件
      ;; (既然不是 "R"，只要 sel 有值，就代表確實選到了物件串列，不會再被 nil 誤導)
      (sel
       (setq ent (car sel))
       (setq entData (entget ent))
       (setq objType (cdr (assoc 0 entData)))

       ;; 3. 檢查是否為文字
       (if (wcmatch objType "*TEXT") ; 支援 TEXT 和 MTEXT
         (progn
           ;; 4. 修改內容 (轉成字串)
           (setq currentStr (itoa startNum))
           (setq entData (subst (cons 1 currentStr) (assoc 1 entData) entData))
           
           (entmod entData)
           (entupd ent)
           
           ;; 5. 數字 +1
           (setq startNum (1+ startNum))
         )
         (princ "\n汪汪！這不是文字喔！請重新點選。")
       )
      )
      
      ;; 狀況 C: 沒選到東西 (點到空白處)，sel 為 nil，且 ERRNO 為 7
      ((= err 7)
       (princ "\n汪汪！沒點到東西喔，請再點一次～")
      )
      
      ;; 狀況 D: 按 Enter 或 空白鍵結束，ERRNO 為 52 -> 結束迴圈
      (T 
       (setq loopActive nil)
      )
    )
  )

  (setvar "CMDECHO" oldEcho)
  
  ;; 載入/結束回報
  (princ "\n汪汪！NN (連續跳號) 已順利結束！")
  (princ)
)