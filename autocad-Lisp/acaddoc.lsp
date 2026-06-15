;;; acaddoc.lsp - Hsiang_Toolbox 自動啟動檔 (含載入檢查)
;;; 每次開啟圖檔會自動執行

(defun s::startup ()
  (princ "\n==============================================")
  (princ "\n   汪汪！Hsiang_Toolbox 系統啟動中...")
  (princ "\n==============================================")
  (Hsiang_AutoLoader)
  (princ "\n")
)

(defun Hsiang_AutoLoader (/ path files file loadedCount)
  (vl-load-com)
  
  ;; 1. 設定 Lisp 資料夾路徑
  (setq path "C:\\Hsiang_Toolbox\\Lisp\\")
  (setq loadedCount 0)

  ;; 2. 搜尋資料夾內所有 .lsp 檔案
  (setq files (vl-directory-files path "*.lsp" 1))

  (if files
    (foreach file files
      ;; 顯示正在載入哪個檔案，不換行
      (princ (strcat "\n[載入] " file " ...... "))
      
      ;; 3. 嘗試載入 (使用 if 判斷成功或失敗)
      (if (load (strcat path file) nil) 
        (progn
          (princ "OK! (成功)") ;; 載入成功顯示這個
          (setq loadedCount (1+ loadedCount))
        )
        (princ "Error! (失敗 - 請檢查程式碼)") ;; 載入失敗顯示這個
      )
    )
    (princ "\n[警告] 找不到任何 .lsp 檔案，請檢查 Lisp 資料夾！")
  )

  (princ (strcat "\n----------------------------------------------"))
  (princ (strcat "\n報告：共載入 " (itoa loadedCount) " 個程式。輸入指令 (CT, DA, LL) 即可使用。"))
  (princ "\n==============================================")
  (princ)
)

;; 立即執行一次 (確保第一次存檔後也能馬上生效)
(Hsiang_AutoLoader)
