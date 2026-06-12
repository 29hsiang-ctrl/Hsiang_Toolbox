;;; -----------------------------------------------------------
;;; 指令: UP (Unfold Plate - 7 Step Mastery + 折耳功能)
;;; 功能: 烤漆板展開工具 (含折耳特徵、純尺寸分段標註)
;;; -----------------------------------------------------------

(vl-load-com)

;; --- [底層數學庫] AutoLISP 缺乏反餘弦，手動定義以防報錯 ---
(defun acos (x)
  (cond
    ((>= x 1.0) 0.0)
    ((<= x -1.0) pi)
    (t (- (/ pi 2.0) (atan (/ x (sqrt (- 1.0 (* x x)))))))
  )
)

;; --- [輔助] 抓取線段索引 ---
(defun get-seg-idx (obj pt / param)
  (setq param (vlax-curve-getParamAtPoint obj (vlax-curve-getClosestPointTo obj pt)))
  (if (and (= (rem param 1.0) 0.0) (> param 0.0))
    (fix (1- param))
    (fix param)
  )
)

;; --- [輔助] 計算兩條線的夾角 ---
(defun get-angle-from-lines ( / sel1 obj1 p1s p1e v1 sel2 obj2 p2s p2e v2 dot mag1 mag2 cosA ang_rad ang_deg)
  (setq sel1 nil)
  (while (null sel1) (setq sel1 (entsel "\n[輔助] 請指定第一條線段: ")))
  (setq obj1 (vlax-ename->vla-object (car sel1)))
  (setq p1s (vlax-curve-getStartPoint obj1) p1e (vlax-curve-getEndPoint obj1))
  (setq v1 (list (- (car p1e) (car p1s)) (- (cadr p1e) (cadr p1s))))

  (setq sel2 nil)
  (while (null sel2) (setq sel2 (entsel "\n[輔助] 請指定第二條線段: ")))
  (setq obj2 (vlax-ename->vla-object (car sel2)))
  (setq p2s (vlax-curve-getStartPoint obj2) p2e (vlax-curve-getEndPoint obj2))
  (setq v2 (list (- (car p2e) (car p2s)) (- (cadr p2e) (cadr p2s))))

  (setq dot (+ (* (car v1) (car v2)) (* (cadr v1) (cadr v2))))
  (setq mag1 (sqrt (+ (* (car v1) (car v1)) (* (cadr v1) (cadr v1)))))
  (setq mag2 (sqrt (+ (* (car v2) (car v2)) (* (cadr v2) (cadr v2)))))
  
  (if (and (> mag1 0) (> mag2 0))
    (progn
      (setq cosA (/ dot (* mag1 mag2)))
      (setq ang_rad (acos cosA))
      (setq ang_deg (* ang_rad (/ 180.0 pi)))
      (if (> ang_deg 90.0) (setq ang_deg (- 180.0 ang_deg))) ; 取銳角
      (princ (strcat "\n-> 測得夾角為: " (rtos ang_deg 2 2) " 度"))
      ang_deg
    )
    45.0
  )
)

(defun c:UP ( / oldEcho oldOs selPoly objPoly pickPt numSegs dists i selPlane idx_plane selSoffit idx_soffit selCancel idx_cancel startPt endPt isReversed new_dists j ang_input ang tanA L ins x0 y0 totalW pTL pTR pBL pBR tempBox rad active_corner cTL cTR cBL cBR loop ptClick minD dX_plane dX_soffit ptsLeft ptsRight curY k get-offsets off_top off_bot X_top_L X_top_R X_bot_L X_bot_R ptL ptR minX dimX dimY_start dimY_end dimY_out_top dimY_in_top botY dimY_out_bot dimY_in_bot idx_ears selEar earIdx new_ears ptTL ptBL ptTR ptBR Y_t Y_b Mid_X_L Mid_X_R span start_X end_X dist_X n_spaces step_X m cx draw-ear mainPtsLeft mainPtsRight)
  (setq oldEcho (getvar "CMDECHO") oldOs (getvar "OSMODE"))
  (setvar "CMDECHO" 0)
  (princ "\n汪汪！執行 UP (終極 7 步展開法則版 + 折耳)...")

  ;; --- [1] 定義整個斷面線 ---
  (setq selPoly nil)
  (while (null selPoly) (setq selPoly (entsel "\n[1] 定義整個斷面線(聚合線): ")))
  (setq objPoly (vlax-ename->vla-object (car selPoly)) pickPt (cadr selPoly))
  
  (setq numSegs (fix (vlax-curve-getEndParam objPoly)) dists '() i 1)
  (while (<= i numSegs)
    (setq dists (append dists (list (- (vlax-curve-getDistAtParam objPoly i) (vlax-curve-getDistAtParam objPoly (1- i))))))
    (setq i (1+ i))
  )

  ;; --- [2] 定義平面 ---
  (setq selPlane nil)
  (while (null selPlane) (setq selPlane (entsel "\n[2] 定義平面的斷面線段: ")))
  (setq idx_plane (get-seg-idx objPoly (cadr selPlane)))

  ;; --- [3] 定義倒吊 ---
  (setq selSoffit (entsel "\n[3] 定義倒吊的斷面線段 (按 Enter 略過): "))
  (setq idx_soffit (if selSoffit (get-seg-idx objPoly (cadr selSoffit)) -1))

  ;; --- [3.5] 定義折耳 (新增功能) ---
  (setq idx_ears '())
  (princ "\n[3.5] 定義折耳的斷面線段 (可連續點選，按 Enter 結束): ")
  (while (setq selEar (entsel "\n選取折耳線段: "))
    (setq earIdx (get-seg-idx objPoly (cadr selEar)))
    (if (not (member earIdx idx_ears))
      (setq idx_ears (append idx_ears (list earIdx)))
    )
  )

  ;; --- [4] 取消板厚 ---
  (setq selCancel (entsel "\n[4] 取消板厚的斷面線段 (按 Enter 略過): "))
  (setq idx_cancel (if selCancel (get-seg-idx objPoly (cadr selCancel)) -1))

  ;; --- 處理方向反轉 (確保展開圖由上往下畫) ---
  (setq startPt (vlax-curve-getStartPoint objPoly) endPt (vlax-curve-getEndPoint objPoly))
  (setq isReversed (> (distance pickPt startPt) (distance pickPt endPt)))
  (if isReversed
    (progn
      (setq dists (reverse dists))
      (setq idx_plane (- numSegs 1 idx_plane))
      (if (>= idx_soffit 0) (setq idx_soffit (- numSegs 1 idx_soffit)))
      (if (>= idx_cancel 0) (setq idx_cancel (- numSegs 1 idx_cancel)))
      (if idx_ears (setq idx_ears (mapcar '(lambda (x) (- numSegs 1 x)) idx_ears)))
    )
  )

  ;; --- 執行移除板厚 ---
  (if (>= idx_cancel 0)
    (progn
      (setq new_dists '() j 0)
      (while (< j numSegs)
        (if (/= j idx_cancel) (setq new_dists (append new_dists (list (nth j dists)))))
        (setq j (1+ j))
      )
      (setq dists new_dists numSegs (1- numSegs))
      (if (> idx_plane idx_cancel) (setq idx_plane (1- idx_plane)))
      (if (> idx_soffit idx_cancel) (setq idx_soffit (1- idx_soffit)))
      (if (= idx_plane idx_cancel) (setq idx_plane -1))
      (if (= idx_soffit idx_cancel) (setq idx_soffit -1))
      
      ;; 更新折耳的索引
      (if idx_ears
        (progn
          (setq new_ears '())
          (foreach x idx_ears
            (cond
              ((< x idx_cancel) (setq new_ears (append new_ears (list x))))
              ((> x idx_cancel) (setq new_ears (append new_ears (list (1- x)))))
            )
          )
          (setq idx_ears new_ears)
        )
      )
    )
  )

  ;; --- [5] 定義切板角度 ---
  (initget 128)
  (setq ang_input (getstring "\n[5] 定義切板角度 (輸入數值 或 輸入 S 指定兩線夾角) <45>: "))
  (cond
    ((= (strcase ang_input) "S") (setq ang (get-angle-from-lines)))
    ((/= ang_input "") (setq ang (atof ang_input)) (if (= ang 0.0) (setq ang 45.0)))
    (t (setq ang 45.0))
  )
  (setq tanA (/ (sin (* ang (/ pi 180.0))) (cos (* ang (/ pi 180.0)))))

  ;; --- [6] 定義總長度 ---
  (setq L (getdist "\n[6] 定義總長度 L= (輸入尺寸 或 點擊兩點): "))
  (while (null L) (setq L (getdist "\n[警告] 必須定義總長度 L=: ")))

  ;; --- [7] 定義平面夾角位置 (四角圓圈單選) ---
  (setq ins (getpoint "\n[7] 點選空白處放置展開圖: "))
  (if ins
    (progn
      (setvar "OSMODE" 0)
      (setq x0 (car ins) y0 (cadr ins) totalW (apply '+ dists))

      ;; 畫出正方形預覽框
      (setq pTL (list x0 y0 0.0) pTR (list (+ x0 L) y0 0.0) pBL (list x0 (- y0 totalW) 0.0) pBR (list (+ x0 L) (- y0 totalW) 0.0))
      (command "_.PLINE" "_non" pTL "_non" pTR "_non" pBR "_non" pBL "_C")
      (setq tempBox (entlast))
      (command "_.CHPROP" tempBox "" "_C" "8" "")

      (setq active_corner 'TR rad (max 5.0 (* totalW 0.04)))
      (setq cTL nil cTR nil cBL nil cBR nil)

      (defun update-circles ()
        (foreach c (list cTL cTR cBL cBR) (if c (entdel c)))
        (command "_.CIRCLE" "_non" pTL rad) (setq cTL (entlast)) (command "_.CHPROP" cTL "" "_C" (if (= active_corner 'TL) "1" "8") "")
        (command "_.CIRCLE" "_non" pTR rad) (setq cTR (entlast)) (command "_.CHPROP" cTR "" "_C" (if (= active_corner 'TR) "1" "8") "")
        (command "_.CIRCLE" "_non" pBL rad) (setq cBL (entlast)) (command "_.CHPROP" cBL "" "_C" (if (= active_corner 'BL) "1" "8") "")
        (command "_.CIRCLE" "_non" pBR rad) (setq cBR (entlast)) (command "_.CHPROP" cBR "" "_C" (if (= active_corner 'BR) "1" "8") "")
      )
      (update-circles)

      (setq loop T)
      (princ "\n預覽正方形。請選取平面夾角位置 (單選)。倒吊將自動執行同側上下相反鏡射！")
      (while loop
        (initget 128 "TL TR BL BR")
        (setq ptClick (getpoint "\n選取夾角 [左上TL/右上TR/左下BL/右下BR] 或點擊圓圈 (Enter確認生成): "))
        (cond
          ((null ptClick) (setq loop nil))
          ((= (type ptClick) 'STR)
            (cond
              ((= (strcase ptClick) "TL") (setq active_corner 'TL))
              ((= (strcase ptClick) "TR") (setq active_corner 'TR))
              ((= (strcase ptClick) "BL") (setq active_corner 'BL))
              ((= (strcase ptClick) "BR") (setq active_corner 'BR))
            )
            (update-circles)
          )
          ((= (type ptClick) 'LIST)
            (setq minD (min (distance ptClick pTL) (distance ptClick pTR) (distance ptClick pBL) (distance ptClick pBR)))
            (cond
              ((equal minD (distance ptClick pTL) 1e-4) (setq active_corner 'TL))
              ((equal minD (distance ptClick pTR) 1e-4) (setq active_corner 'TR))
              ((equal minD (distance ptClick pBL) 1e-4) (setq active_corner 'BL))
              ((equal minD (distance ptClick pBR) 1e-4) (setq active_corner 'BR))
            )
            (update-circles)
          )
        )
      )
      
      ;; 清除預覽
      (foreach c (list tempBox cTL cTR cBL cBR) (if c (entdel c)))

      ;; ==============================================================
      ;; 生成展開圖：全域偏移映射法 (確保長方形無縫相連)
      ;; 完美支援：同側上下相反 (TL->BL, BL->TL, TR->BR, BR->TR)
      ;; ==============================================================
      (setq dX_plane (if (>= idx_plane 0) (* (nth idx_plane dists) tanA) 0.0))
      (setq dX_soffit (if (>= idx_soffit 0) (* (nth idx_soffit dists) tanA) 0.0))

      ;; 計算第 k 條水平折線的精準 X 軸偏移量
      (defun get-offsets (k / off_L off_R)
        (setq off_L 0.0 off_R 0.0)
        ;; 1. 平面造成的偏移 (選哪裡，尖端就在哪裡)
        (cond
          ((= active_corner 'TL) (if (> k idx_plane) (setq off_L (+ off_L dX_plane))))
          ((= active_corner 'TR) (if (> k idx_plane) (setq off_R (- off_R dX_plane))))
          ((= active_corner 'BL) (if (<= k idx_plane) (setq off_L (+ off_L dX_plane))))
          ((= active_corner 'BR) (if (<= k idx_plane) (setq off_R (- off_R dX_plane))))
        )
        ;; 2. 倒吊造成的偏移 (同側上下相反: TL觸發BL, TR觸發BR, BL觸發TL, BR觸發TR)
        (if (>= idx_soffit 0)
          (cond
            ((= active_corner 'TL) (if (> k idx_soffit) (setq off_L (- off_L dX_soffit))))
            ((= active_corner 'TR) (if (> k idx_soffit) (setq off_R (+ off_R dX_soffit))))
            ((= active_corner 'BL) (if (> k idx_soffit) (setq off_L (+ off_L dX_soffit))))
            ((= active_corner 'BR) (if (> k idx_soffit) (setq off_R (- off_R dX_soffit))))
          )
        )
        (list off_L off_R)
      )

      (setq ptsLeft '() ptsRight '() curY y0 i 0)
      
      (while (< i numSegs)
        (setq Y_top curY Y_bot (- curY (nth i dists)))

        (setq off_top (get-offsets i))
        (setq off_bot (get-offsets (1+ i)))

        (setq X_top_L (+ x0 (car off_top)))
        (setq X_top_R (+ x0 L (cadr off_top)))
        (setq X_bot_L (+ x0 (car off_bot)))
        (setq X_bot_R (+ x0 L (cadr off_bot)))

        (setq ptsLeft (append ptsLeft (list (list X_top_L Y_top 0.0) (list X_bot_L Y_bot 0.0))))
        (setq ptsRight (append ptsRight (list (list X_top_R Y_top 0.0) (list X_bot_R Y_bot 0.0))))

        (setq curY Y_bot i (1+ i))
      )

      ;; --- 繪製外框輪廓 (排除折耳段) ---
      (setq mainPtsLeft '() mainPtsRight '())
      (setq k 0)
      (while (< k numSegs)
        (if (not (member k idx_ears))
          (progn
            (setq mainPtsLeft (append mainPtsLeft (list (nth (* k 2) ptsLeft) (nth (+ (* k 2) 1) ptsLeft))))
            (setq mainPtsRight (append mainPtsRight (list (nth (* k 2) ptsRight) (nth (+ (* k 2) 1) ptsRight))))
          )
        )
        (setq k (1+ k))
      )

      (if mainPtsLeft
        (progn
          (command "_.PLINE")
          (foreach pt mainPtsLeft (command "_non" pt))
          (foreach pt (reverse mainPtsRight) (command "_non" pt))
          (command "_C")
          (command "_.CHPROP" (entlast) "" "_C" "4" "")
        )
      )

      ;; --- 繪製內部折彎線 (若相鄰兩段皆非折耳才畫) ---
      (setq k 0)
      (while (< k (1- numSegs))
        (if (and (not (member k idx_ears)) (not (member (1+ k) idx_ears)))
          (progn
            (setq ptL (nth (+ (* k 2) 1) ptsLeft))
            (setq ptR (nth (+ (* k 2) 1) ptsRight))
            (command "_.LINE" "_non" ptL "_non" ptR "")
            (command "_.CHPROP" (entlast) "" "_C" "4" "")
          )
        )
        (setq k (1+ k))
      )

      ;; --- [新增] 繪製折耳 (方框與圓孔) ---
      (defun draw-ear (cx yt yb / p1 p2 p3 p4 cmid)
        (setq p1 (list (- cx 15.0) yt 0.0)
              p2 (list (+ cx 15.0) yt 0.0)
              p3 (list (+ cx 15.0) yb 0.0)
              p4 (list (- cx 15.0) yb 0.0))
        (command "_.PLINE" "_non" p1 "_non" p2 "_non" p3 "_non" p4 "_C")
        (command "_.CHPROP" (entlast) "" "_C" "4" "") ;; 方框為青色
        (setq cmid (list cx (/ (+ yt yb) 2.0) 0.0))
        (command "_.CIRCLE" "_non" cmid 2.5)          ;; 半徑2.5=直徑5
        (command "_.CHPROP" (entlast) "" "_C" "3" "") ;; 圓圈為綠色
      )

      (setq k 0)
      (while (< k numSegs)
        (if (member k idx_ears)
          (progn
            (setq ptTL (nth (* k 2) ptsLeft))
            (setq ptBL (nth (+ (* k 2) 1) ptsLeft))
            (setq ptTR (nth (* k 2) ptsRight))
            (setq ptBR (nth (+ (* k 2) 1) ptsRight))
            
            (setq Y_t (cadr ptTL) Y_b (cadr ptBL))
            (setq Mid_X_L (/ (+ (car ptTL) (car ptBL)) 2.0))
            (setq Mid_X_R (/ (+ (car ptTR) (car ptBR)) 2.0))
            (setq span (- Mid_X_R Mid_X_L))
            
            ;; 若總寬度過小(<=200)，則只在正中間畫一個
            (if (<= span 200.0)
              (draw-ear (/ (+ Mid_X_L Mid_X_R) 2.0) Y_t Y_b)
              (progn
                (setq start_X (+ Mid_X_L 100.0))
                (setq end_X (- Mid_X_R 100.0))
                (setq dist_X (- end_X start_X))
                
                ;; 計算陣列數量 (間距約 400)
                (setq n_spaces (fix (+ (/ dist_X 400.0) 0.5)))
                (if (< n_spaces 1) (setq n_spaces 1))
                (setq step_X (/ dist_X (float n_spaces)))
                
                (setq m 0)
                (while (<= m n_spaces)
                  (setq cx (+ start_X (* m step_X)))
                  (draw-ear cx Y_t Y_b)
                  (setq m (1+ m))
                )
              )
            )
          )
        )
        (setq k (1+ k))
      )

      ;; ==============================================================
      ;; 全自動標註系統 (總長、分段水平向長、左側垂直寬)
      ;; ==============================================================
      
      ;; 1. 繪製上方水平標註
      (setq dimY_out_top (+ y0 (* L 0.14)) dimY_in_top (+ y0 (* L 0.07)))
      
      (command "_.DIMLINEAR" "_non" (list x0 y0 0) "_non" (list (+ x0 L) y0 0) "_H" "_non" (list x0 dimY_out_top 0))
      (command "_.CHPROP" (entlast) "" "_C" "1" "")
      
      (if (or (= active_corner 'TR) (= active_corner 'BR))
        (progn
          (command "_.DIMLINEAR" "_non" (list x0 y0 0) "_non" (list (- (+ x0 L) dX_plane) y0 0) "_H" "_non" (list x0 dimY_in_top 0))
          (command "_.CHPROP" (entlast) "" "_C" "1" "")
          (command "_.DIMLINEAR" "_non" (list (- (+ x0 L) dX_plane) y0 0) "_non" (list (+ x0 L) y0 0) "_H" "_non" (list x0 dimY_in_top 0))
          (command "_.CHPROP" (entlast) "" "_C" "1" "")
        )
        (progn
          (command "_.DIMLINEAR" "_non" (list x0 y0 0) "_non" (list (+ x0 dX_plane) y0 0) "_H" "_non" (list x0 dimY_in_top 0))
          (command "_.CHPROP" (entlast) "" "_C" "1" "")
          (command "_.DIMLINEAR" "_non" (list (+ x0 dX_plane) y0 0) "_non" (list (+ x0 L) y0 0) "_H" "_non" (list x0 dimY_in_top 0))
          (command "_.CHPROP" (entlast) "" "_C" "1" "")
        )
      )

      ;; 2. 繪製下方水平標註
      (setq botY (- y0 totalW) dimY_out_bot (- botY (* L 0.14)) dimY_in_bot (- botY (* L 0.07)))
      (if (>= idx_soffit 0)
        (progn
          (command "_.DIMLINEAR" "_non" (list x0 botY 0) "_non" (list (+ x0 L) botY 0) "_H" "_non" (list x0 dimY_out_bot 0))
          (command "_.CHPROP" (entlast) "" "_C" "1" "")
          (if (or (= active_corner 'TR) (= active_corner 'BR))
            (progn
              (command "_.DIMLINEAR" "_non" (list x0 botY 0) "_non" (list (- (+ x0 L) dX_soffit) botY 0) "_H" "_non" (list x0 dimY_in_bot 0))
              (command "_.CHPROP" (entlast) "" "_C" "1" "")
              (command "_.DIMLINEAR" "_non" (list (- (+ x0 L) dX_soffit) botY 0) "_non" (list (+ x0 L) botY 0) "_H" "_non" (list x0 dimY_in_bot 0))
              (command "_.CHPROP" (entlast) "" "_C" "1" "")
            )
            (progn
              (command "_.DIMLINEAR" "_non" (list x0 botY 0) "_non" (list (+ x0 dX_soffit) botY 0) "_H" "_non" (list x0 dimY_in_bot 0))
              (command "_.CHPROP" (entlast) "" "_C" "1" "")
              (command "_.DIMLINEAR" "_non" (list (+ x0 dX_soffit) botY 0) "_non" (list (+ x0 L) botY 0) "_H" "_non" (list x0 dimY_in_bot 0))
              (command "_.CHPROP" (entlast) "" "_C" "1" "")
            )
          )
        )
        (progn
          (command "_.DIMLINEAR" "_non" (list x0 botY 0) "_non" (list (+ x0 L) botY 0) "_H" "_non" (list x0 dimY_out_bot 0))
          (command "_.CHPROP" (entlast) "" "_C" "1" "")
        )
      )

      ;; 3. 繪製左側垂直寬度
      (setq minX (apply 'min (mapcar 'car ptsLeft)))
      (setq dimX (- minX (* L 0.05)) dimY_start y0)
      
      (foreach w dists
        (setq dimY_end (- dimY_start w))
        (command "_.DIMLINEAR" "_non" (list minX dimY_start 0.0) "_non" (list minX dimY_end 0.0) "_V" "_non" (list dimX y0 0.0))
        (command "_.CHPROP" (entlast) "" "_C" "1" "")
        (setq dimY_start dimY_end)
      )

      (princ "\n汪汪！完美展開圖已生成！純尺寸自動標註與折耳繪製完成！")
    )
  )

  (setvar "OSMODE" oldOs)
  (setvar "CMDECHO" oldEcho)
  (princ)
)

(princ "\n汪汪！UP (純尺寸標註 + 折耳) 載入成功！")
(princ)