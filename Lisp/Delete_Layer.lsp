;;; -----------------------------------------------------------
;;; 指令: DL (Delete Layer)
;;; 功能: 選取物件，刪除其所屬圖層上的「所有物件」並「刪除圖層」
;;; 安全機制: 
;;; 1. 自動跳過圖層 0 和 Defpoints
;;; 2. 若刪除的是目前圖層，會自動切換回 0 層
;;; 3. 操作前會彈出確認警告
;;; -----------------------------------------------------------

(defun c:DL (/ ss i ent layName layerList uniqueLayers ans ssDel delCount oldEcho)
  (vl-load-com)
  (setq oldEcho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (princ "\n汪汪！執行 DL (圖層毀滅者)...")

  ;; 1. 選取範本物件
  (princ "\n請選取要刪除的圖層上的物件 (選誰，誰的家族就滅亡): ")
  (setq ss (ssget))

  (if ss
    (progn
      ;; 2. 收集圖層名稱
      (setq layerList '())
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (setq layName (cdr (assoc 8 (entget ent))))
        (setq layerList (cons layName layerList))
        (setq i (1+ i))
      )
      
      ;; 過濾重複並移除 0 和 Defpoints
      (setq uniqueLayers '())
      (foreach x layerList
        (if (and (not (member x uniqueLayers))
                 (/= (strcase x) "0")
                 (/= (strcase x) "DEFPOINTS"))
          (setq uniqueLayers (cons x uniqueLayers))
        )
      )

      (if uniqueLayers
        (progn
          ;; 顯示將被刪除的圖層
          (princ "\n\n準備刪除以下圖層及其所有內容: ")
          (foreach x uniqueLayers (princ (strcat "\n - " x)))
          
          ;; 3. 安全確認
          (initget "Yes No")
          (setq ans (getkword "\n\n警告！這將刪除圖面上該圖層的所有物件！確定嗎? [Yes/No] <No>: "))
          
          (if (= ans "Yes")
            (progn
              (setq delCount 0)
              
              ;; 4. 開始執行迴圈
              (foreach lay uniqueLayers
                ;; A. 檢查是否為當前圖層，是的話切換到 0
                (if (= (strcase (getvar "CLAYER")) (strcase lay))
                  (setvar "CLAYER" "0")
                )
                
                ;; B. 選取該圖層所有物件並刪除
                (setq ssDel (ssget "_X" (list (cons 8 lay))))
                (if ssDel
                  (progn
                    (command "_.ERASE" ssDel "")
                    (princ (strcat "\n[清空] 已刪除圖層 " lay " 上的物件。"))
                  )
                )

                ;; C. 清理 (Purge) 該圖層
                ;; 使用 -PURGE 指令嘗試刪除圖層定義
                (command "_.-PURGE" "_LA" lay "_N")
                
                ;; 檢查圖層是否真的消失 (如果圖塊內有引用，Purge 會失敗，這是保護機制)
                (if (tblsearch "LAYER" lay)
                  (princ (strcat "\n[保留] 圖層 " lay " 內容已刪，但圖層本身被圖塊佔用，無法移除。"))
                  (progn
                    (princ (strcat "\n[刪除] 圖層 " lay " 已完全移除。"))
                    (setq delCount (1+ delCount))
                  )
                )
              )
              
              (princ (strcat "\n\n汪汪！任務完成。共完全移除 " (itoa delCount) " 個圖層。"))
            )
            (princ "\n使用者取消操作。呼~ 嚇死寶寶了。")
          )
        )
        (princ "\n\n[提示] 你選到的都是 0 層或 Defpoints，這些是系統圖層，不能刪除喔！")
      )
    )
    (princ "\n未選取任何物件。")
  )

  (setvar "CMDECHO" oldEcho)
  (princ "\n汪汪！DL (圖層刪除) 已載入！")
  (princ)
)