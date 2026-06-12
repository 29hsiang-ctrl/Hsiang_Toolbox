;;; -----------------------------------------------------------
;;; 指令: LD (Text Leader)
;;; 功能: 快速建立多重引線，自訂文字內容、字高與箭頭大小
;;; -----------------------------------------------------------

(defun c:LD (/ txt th aSize pt1 pt2 oldEcho mldObj)
  (vl-load-com)
  (setq oldEcho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (princ "\n汪汪！執行 LD (快速多重引線)...")

  ;; --- 1. 輸入文字 (允許輸入空格) ---
  (setq txt (getstring T "\n請輸入文字內容: "))

  (if (and txt (/= txt ""))
    (progn
      ;; --- 2. 輸入文字高度 ---
      (if (null *Hsiang_LD_TH*) (setq *Hsiang_LD_TH* 2.5))
      (setq th (getdist (strcat "\n請輸入文字高度 <" (rtos *Hsiang_LD_TH* 2 2) ">: ")))
      (if (null th) (setq th *Hsiang_LD_TH*) (setq *Hsiang_LD_TH* th))

      ;; --- 3. 輸入箭頭大小 ---
      (if (null *Hsiang_LD_AS*) (setq *Hsiang_LD_AS* 10.0))
      (setq aSize (getdist (strcat "\n請輸入箭頭大小 <" (rtos *Hsiang_LD_AS* 2 2) ">: ")))
      (if (null aSize) (setq aSize *Hsiang_LD_AS*) (setq *Hsiang_LD_AS* aSize))

      ;; --- 4. 點選位置並繪製 ---
      (setq pt1 (getpoint "\n請點選「箭頭」位置 (起點): "))
      (if pt1
        (progn
          (setq pt2 (getpoint pt1 "\n請點選「文字」位置 (終點): "))
          (if pt2
            (progn
              ;; 使用內建指令建立多重引線 (先塞入假文字 "X" 避免指令列遇到空白字元斷掉)
              (command "_.MLEADER" pt1 pt2 "X")

              ;; 取得剛剛畫出來的 MLeader 物件
              (setq mldObj (vlax-ename->vla-object (entlast)))

              ;; 透過 VLA 屬性強制覆蓋設定
              (vla-put-TextString mldObj txt)        ; 替換成真正的文字
              (vla-put-TextHeight mldObj th)         ; 設定字高
              (vla-put-ArrowheadSize mldObj aSize)   ; 設定箭頭大小

              (princ "\n成功！多重引線已建立。")
            )
            (princ "\n取消操作。")
          )
        )
        (princ "\n取消操作。")
      )
    )
    (princ "\n未輸入文字，取消操作。")
  )

  (setvar "CMDECHO" oldEcho)
  (princ "\n汪汪！LD (快速引線) 已載入！")
  (princ)
)