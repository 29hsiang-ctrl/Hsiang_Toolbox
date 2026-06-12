;;; -----------------------------------------------------------
;;; 指令: LX (LockXref)
;;; 功能: 集中管理外部參考 (Xref)
;;; 動作: 
;;; 1. 建立圖層 "0-外部參考"
;;; 2. 將所有 Xref 物件搬移至該圖層
;;; 3. 鎖定該圖層
;;; -----------------------------------------------------------

(defun c:LX (/ ss i ent obj blkName blkDef layObj layers targetLayName sourceLayName sourceLayObj count moveCount)
  (vl-load-com)
  (setvar "CMDECHO" 0)
  (princ "\n汪汪！正在執行 Xref 集中管理與鎖定...")

  (setq targetLayName "0-外部參考")
  (setq layers (vla-get-layers (vla-get-activedocument (vlax-get-acad-object))))
  (setq count 0)
  (setq moveCount 0)

  ;; --- 1. 準備目標圖層 ---
  ;; 檢查圖層是否存在，若無則建立
  (if (tblsearch "LAYER" targetLayName)
    (setq layObj (vla-item layers targetLayName)) ; 存在則抓取
    (setq layObj (vla-add layers targetLayName))  ; 不存在則建立
  )
  
  ;; 先確保目標圖層是「解鎖」狀態，不然無法搬東西進去
  (vla-put-Lock layObj :vlax-false)
  ;; 設定顏色為灰色 (8號) 比較好辨識底圖 (選用，可自行修改)
  (vla-put-Color layObj 8) 

  ;; --- 2. 掃描與搬移 ---
  (setq ss (ssget "_X" '((0 . "INSERT"))))

  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (setq obj (vlax-ename->vla-object ent))
        
        ;; 獲取圖塊名稱
        (if (vlax-property-available-p obj 'Name)
          (progn
            (setq blkName (vla-get-Name obj))
            (setq blkDef (tblsearch "BLOCK" blkName))
            
            ;; 檢查是否為 Xref (群組碼 70 的第 4 位元)
            (if (and blkDef (= (logand (cdr (assoc 70 blkDef)) 4) 4))
              (progn
                ;; 檢查物件目前的圖層是否被鎖定，若鎖定要先解鎖才能搬
                (setq sourceLayName (vla-get-Layer obj))
                (setq sourceLayObj (vla-item layers sourceLayName))
                
                (if (= (vla-get-Lock sourceLayObj) :vlax-true)
                  (vla-put-Lock sourceLayObj :vlax-false) ;; 暫時解鎖來源層
                )

                ;; ★ 關鍵動作：搬移圖層 ★
                (vla-put-Layer obj targetLayName)
                (setq moveCount (1+ moveCount))
                (princ (strcat "\n[搬移] Xref: " blkName " -> 移至 " targetLayName))
              )
            )
          )
        )
        (setq i (1+ i))
      )
    )
    (princ "\n圖面上找不到外部參考。")
  )

  ;; --- 3. 鎖定目標圖層 ---
  (if (> moveCount 0)
    (progn
      (vla-put-Lock layObj :vlax-true)
      (princ (strcat "\n\n汪汪！任務完成！\n已將 " (itoa moveCount) " 個外部參考移至圖層 [" targetLayName "] 並已鎖定。"))
      (princ "\n現在這些底圖很安全，不會被刪除了！")
    )
    (princ "\n\n沒有發現任何外部參考，無需動作。")
  )
  
  (setvar "CMDECHO" 1)
  
  ;; 載入回報
  (princ "\n汪汪！LX (外部參考集中鎖定) 已載入！")
  (princ)
)