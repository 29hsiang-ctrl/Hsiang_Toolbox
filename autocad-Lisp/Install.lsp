;;; Install.lsp - 強制更新版
;;; 自動刪除舊的 .cuix 以強制重新編譯選單
;;; 汪汪！

(defun c:HsiangInstall (/ acadObj prefObj supportPaths newPath menuPath cuixPath trustPaths newTrust err)
  (vl-load-com)
  (princ "\n汪汪！開始安裝 (強制更新模式)...")

  (setq basePath "C:\\Hsiang_Toolbox")
  (setq iconPath "C:\\Hsiang_Toolbox\\Icons")

  (setq acadObj (vlax-get-acad-object))
  (setq prefObj (vla-get-Files (vla-get-Preferences acadObj)))

  ;; 1. 設定支援路徑
  (setq supportPaths (vlax-get-property prefObj "SupportPath"))
  (if (not (vl-string-search basePath supportPaths))
    (vlax-put-property prefObj "SupportPath" (strcat basePath ";" iconPath ";" supportPaths))
  )

  ;; 2. 設定受信任路徑 (防錯)
  (setq err (vl-catch-all-apply 
    '(lambda ()
       (setq trustPaths (vlax-get-property prefObj "TrustedPaths"))
       (if (not (vl-string-search basePath trustPaths))
         (vlax-put-property prefObj "TrustedPaths" (strcat basePath "..." ";" trustPaths))
       )
    )
  ))

  ;; 3. 強制重載選單 (關鍵步驟！)
  (setq menuPath (strcat basePath "\\Menu\\Hsiang_Menu.mnu"))
  (setq cuixPath (strcat basePath "\\Menu\\Hsiang_Menu.cuix"))
  
  ;; A. 先卸載舊選單
  (if (menugroup "HSIANG_TOOLS")
    (progn
      (command "_.MENUUNLOAD" "HSIANG_TOOLS")
      (princ "\n[清理] 已卸載舊選單。")
    )
  )

  ;; B. 刪除舊的 .cuix 編譯檔 (這就是解決空選單的關鍵！)
  (if (findfile cuixPath)
    (progn
      (if (vl-file-delete cuixPath)
        (princ "\n[清理] 已刪除舊的 .cuix 快取檔。")
        (princ "\n[注意] 無法刪除 .cuix (可能被佔用)，請重開 CAD 再試。")
      )
    )
  )

  ;; C. 重新載入 .mnu (這會產生全新的 .cuix)
  (if (findfile menuPath)
    (progn
      (command "_.MENULOAD" menuPath)
      (princ "\n[OK] 選單已重新編譯並掛載！")
    )
    (princ "\n[錯誤] 找不到 .mnu 檔！")
  )

  (alert "安裝完成！\n\n舊的快取已清除。\n請檢查上方功能表是否出現「Hsiang工具箱」。")
  (princ)
)

(c:HsiangInstall)