;;; Install.lsp - First-time setup
;;; Adds support paths and loads the pre-built cuix (which has ribbon content).
;;; Drag this file into AutoCAD to install. Only needs to be run once.
;;; NOTE: Do NOT delete or regenerate the .cuix from .mnu — it would lose ribbon data.

(defun c:HsiangInstall (/ acadObj prefObj supportPaths cuixPath trustPaths err
                          _acadObj _groups _hsiangGroup _menus _popup _mbar)
  (vl-load-com)
  (princ "\nHsiang_Toolbox: Starting installation...")

  (setq basePath "C:\\Hsiang_Toolbox\\autocad-Lisp")
  (setq iconPath "C:\\Hsiang_Toolbox\\autocad-Lisp\\Icons")
  (setq cuixPath (strcat basePath "\\Menu\\Hsiang_Menu.cuix"))

  (setq acadObj (vlax-get-acad-object))
  (setq prefObj (vla-get-Files (vla-get-Preferences acadObj)))

  ;; 1. Add support paths (so acaddoc.lsp is auto-loaded on every startup)
  (setq supportPaths (vlax-get-property prefObj "SupportPath"))
  (if (not (vl-string-search basePath supportPaths))
    (progn
      (vlax-put-property prefObj "SupportPath"
        (strcat basePath ";" iconPath ";" supportPaths))
      (princ "\n[OK] Support paths added.")
    )
    (princ "\n[Skip] Support paths already set.")
  )

  ;; 2. Add trusted paths (suppress security prompts)
  (setq err (vl-catch-all-apply
    '(lambda ()
       (setq trustPaths (vlax-get-property prefObj "TrustedPaths"))
       (if (not (vl-string-search basePath trustPaths))
         (vlax-put-property prefObj "TrustedPaths"
           (strcat basePath ";" trustPaths))
       )
    )
  ))

  ;; 3. Show classic pull-down menu bar
  (setvar "MENUBAR" 1)

  ;; 4. Unload old version if loaded, then load the pre-built cuix
  ;; IMPORTANT: Load .cuix directly — do NOT regenerate from .mnu (ribbon content would be lost)
  (if (menugroup "HSIANG_TOOLS")
    (progn
      (command "_.MENUUNLOAD" "HSIANG_TOOLS")
      (princ "\n[OK] Old menu unloaded.")
    )
  )

  (if (findfile cuixPath)
    (progn
      (command "_.MENULOAD" cuixPath)
      (princ "\n[OK] Hsiang_Menu.cuix loaded.")
    )
    (princ "\n[Error] Hsiang_Menu.cuix not found!")
  )

  ;; 4b. Explicitly insert popup into the classic menu bar via VLA
  (if (menugroup "HSIANG_TOOLS")
    (progn
      (setq _acadObj     (vlax-get-acad-object))
      (setq _groups      (vla-get-MenuGroups _acadObj))
      (setq _hsiangGroup (vla-item _groups "HSIANG_TOOLS"))
      (setq _menus       (vla-get-Menus _hsiangGroup))
      (setq _popup       (vla-item _menus 0))
      (if (not (= :vlax-true (vla-get-OnMenuBar _popup)))
        (progn
          (setq _mbar (vla-get-MenuBar _acadObj))
          (vla-InsertInMenuBar _popup (vla-get-Count _mbar))
          (princ "\n[OK] Hsiang工具 已加入傳統選單列")
        )
        (princ "\n[Skip] 傳統選單列已含 Hsiang工具")
      )
    )
  )


(alert "Installation complete!\n\nHsiang工具 now appears in:\n  - The classic menu bar (top row, with Express / M-拖打印)\n  - The ribbon tab row\n\nThe menu loads automatically on every future startup.")
  (princ)
)

(c:HsiangInstall)
