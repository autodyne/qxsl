;; ALLJA1 CONTEST DEFINED by 無線部開発班

(load "qxsl/ruler/format.lisp")

; UTC-JST conversion for ADIF
(defun 時刻 it (hour (qxsl-time it) "JST"))

; mode validation
(defun 電信? it (match "(?i)CW" (qxsl-mode it)))
(defun 電話? it (match "(?i)PH|AM|FM|[DS]SB" (qxsl-mode it)))
(defun 離散? it (match "(?i)DG|FT4|FT8|RTTY" (qxsl-mode it)))
(defun 電電? it (or (電信? it) (電話? it)))
(defun 総合? it (or (電電? it) (離散? it)))

; time validation
(defun 朝? it (<= 09 (時刻 it) 11))
(defun 昼? it (<= 13 (時刻 it) 14))
(defun 夜? it (<= 16 (時刻 it) 19))

; multiple-band validation
(defun 高? it (<= 14000 (qxsl-band it) 50000))
(defun 低? it (<=  1900 (qxsl-band it)  7000))

; single lower-band validation
(defun 1.9MHz? it (equal (qxsl-band it)  1900))
(defun 3.5MHz? it (equal (qxsl-band it)  3500))
(defun   7MHz? it (equal (qxsl-band it)  7000))

; single UPPER-BAND validation
(defun  14MHz? it (equal (qxsl-band it) 14000))
(defun  21MHz? it (equal (qxsl-band it) 21000))
(defun  28MHz? it (equal (qxsl-band it) 28000))
(defun  50MHz? it (equal (qxsl-band it) 50000))

; validation of analog/digital sections
(defun HBAND? it (forall it (朝? 電電?   高?)))
(defun LBAND? it (forall it (夜? 電電?   低?)))
(defun DIGIT? it (forall it (昼? 離散? 7MHz?)))
(defun ABAND? it (or (HBAND? it) (LBAND? it)))
(defun JOINT? it (or (HBAND? it) (LBAND? it) (DIGIT? it)))

; JCC/JCG
(defun peel-JCCG it
	(cadr
		(split
			(if (電話? it) "^.." "^...")
			(qxsl-code it))))
(defun qxsl-JCCG it
	(if
		(null? (qxsl-rstq it))
		(peel-JCCG it)
		(qxsl-code it)))

; conversion of JCC/JCG to city/prefecture name
(defun 県 it (city "jarl" (qxsl-JCCG it) 0))
(defun 市 it (city "jarl" (qxsl-JCCG it)))
(defun 総 it (city "area" (県 it) 2))

; JCC/JCG validation
(defun 現存? it (not (null? (市 it))))
(defun 支庁? it (match "\\d{3}"  (qxsl-JCCG it)))
(defun 府県? it (match "\\d{2}"  (qxsl-JCCG it)))
(defun 市郡? it (match "\\d{4,}" (qxsl-JCCG it)))

(defun 関東? it (equal (総 it) "関東"))
(defun 北海? it (equal (県 it) "北海道"))
(defun 管外? it (not (or (関東? it) (北海? it))))

;; validation of 1エリア内/1エリア外部門
(defun 内? it
	(and
		(現存? it)
		(cond (
			((離散? it) (市郡? it))
			((北海? it) (支庁? it))
			((関東? it) (市郡? it))
			((管外? it) (府県? it))))))

(defun 外? it (and (現存? it) (関東? it) (市郡? it)))

;; validation of 個人部門/団体部門
(defun SinOP? it #t)
(defun MulOp? it (not (member (qxsl-name it) (list null ""))))

; keys for scoring
(defun CALL it
	(list
		(qxsl-call it)
		(qxsl-band it)
		(cond (
			((電信? it) 1)
			((電話? it) 2)
			((離散? it) 3)))))
(defun MULT it
	(list
		(qxsl-band it)
		(qxsl-JCCG it)))
(defun SOLE it null)
(defun CORP it (qxsl-name it))

; scoring
(defmacro 得点 (score calls mults names)
	`(progn
		(setq mults (length (quote ,mults)))
		(setq names (length (quote ,names)))
		(ceiling (/ (* ,score mults) names))))

; validation routine
(defmacro 検査 (conds keys)
	`(lambda it
		(progn
			(toQXSL it)
			(setq error (unsat it ,conds))
			(if
				(nil? error)
				(success it 1 ,@(dolist (k keys) (list k 'it)))
				(failure it (format "無効な交信 %s" error))))))

(defmacro 個検査 conds `(検査 ,conds (CALL MULT SOLE)))
(defmacro 団検査 conds `(検査 ,conds (CALL MULT CORP)))

; section codes
(setq SinHB "Sin1")
(setq SinLB "Sin2")
(setq SinDG "Sin3")
(setq SinJS "Sin4")
(setq MulAB "Mul1")
(setq MulDG "Mul2")
(setq MulJS "Mul3")

(setq JA1 (contest "ALLJA1 TEST" 得点))

; schedule
(defun starting year (date year "JUNE" "SATURDAY" 4))
(defun deadline year (date year "JULY" "SATURDAY" 3))

; section names
(setq 内 "1エリア内")
(setq 外 "1エリア外")

(setq 個 "個人")
(setq 団 "団体")

(setq 電信 "電信限定")
(setq 電電 "電信電話")
(setq 離散 "デジタル")
(setq 総合 "総合")

(setq 1.9MHz "1.9MHz部門")
(setq 3.5MHz "3.5MHz部門")
(setq   7MHz   "7MHz部門")
(setq  14MHz  "14MHz部門")
(setq  21MHz  "21MHz部門")
(setq  28MHz  "28MHz部門")
(setq  50MHz  "50MHz部門")
(setq LBANDS    "1.9/3.5/7MHz部門")
(setq HBANDS  "14/21/28/50MHz部門")

; routines for ATS-4
(defmacro 総合部門の選択 sects
	`(progn
		(setq sects (dolist (s (list ,@sects)) (split " " s)))
		(setq corps (cond (
			((member 団 (mapcar cadr sects)) 団)
			((member 個 (mapcar cadr sects)) 個))))
		(setq areas (cond (
			((member 内 (mapcar car  sects)) 内)
			((member 外 (mapcar car  sects)) 外))))
		(cat areas corps 総合 "部門")))

(defun 部門と運用地の検査 (部門 市区町村)
	(xor
		(equal (car (split " " 部門)) 外)
		(equal (city "area" (car (split "(?<=都|道|府|県)" 市区町村)) 2) "関東")))

(defmacro cat (area opnum mode band) `(concat " " (list ,area ,opnum ,mode ,band)))

; 1エリア内 個人 ローバンド部門
(section JA1 (cat 内 個 電信 1.9MHz) SinLB (個検査 (SinOP? 内? 電信? 1.9MHz? LBAND?)))
(section JA1 (cat 内 個 電信 3.5MHz) SinLB (個検査 (SinOP? 内? 電信? 3.5MHz? LBAND?)))
(section JA1 (cat 内 個 電電 3.5MHz) SinLB (個検査 (SinOP? 内? 電電? 3.5MHz? LBAND?)))
(section JA1 (cat 内 個 電信   7MHz) SinLB (個検査 (SinOP? 内? 電信?   7MHz? LBAND?)))
(section JA1 (cat 内 個 電電   7MHz) SinLB (個検査 (SinOP? 内? 電電?   7MHz? LBAND?)))
(section JA1 (cat 内 個 電信 LBANDS) SinLB (個検査 (SinOP? 内? 電信?         LBAND?)))
(section JA1 (cat 内 個 電電 LBANDS) SinLB (個検査 (SinOP? 内? 電電?         LBAND?)))

; 1エリア外 個人 ローバンド部門
(section JA1 (cat 外 個 電信 1.9MHz) SinLB (個検査 (SinOP? 外? 電信? 1.9MHz? LBAND?)))
(section JA1 (cat 外 個 電信 3.5MHz) SinLB (個検査 (SinOP? 外? 電信? 3.5MHz? LBAND?)))
(section JA1 (cat 外 個 電電 3.5MHz) SinLB (個検査 (SinOP? 外? 電電? 3.5MHz? LBAND?)))
(section JA1 (cat 外 個 電信   7MHz) SinLB (個検査 (SinOP? 外? 電信?   7MHz? LBAND?)))
(section JA1 (cat 外 個 電電   7MHz) SinLB (個検査 (SinOP? 外? 電電?   7MHz? LBAND?)))
(section JA1 (cat 外 個 電信 LBANDS) SinLB (個検査 (SinOP? 外? 電信?         LBAND?)))
(section JA1 (cat 外 個 電電 LBANDS) SinLB (個検査 (SinOP? 外? 電電?         LBAND?)))

; 1エリア内 個人 ハイバンド部門
(section JA1 (cat 内 個 電信  14MHz) SinHB (個検査 (SinOP? 内? 電信?  14MHz? HBAND?)))
(section JA1 (cat 内 個 電電  14MHz) SinHB (個検査 (SinOP? 内? 電電?  14MHz? HBAND?)))
(section JA1 (cat 内 個 電信  21MHz) SinHB (個検査 (SinOP? 内? 電信?  21MHz? HBAND?)))
(section JA1 (cat 内 個 電電  21MHz) SinHB (個検査 (SinOP? 内? 電電?  21MHz? HBAND?)))
(section JA1 (cat 内 個 電信  28MHz) SinHB (個検査 (SinOP? 内? 電信?  28MHz? HBAND?)))
(section JA1 (cat 内 個 電電  28MHz) SinHB (個検査 (SinOP? 内? 電電?  28MHz? HBAND?)))
(section JA1 (cat 内 個 電信  50MHz) SinHB (個検査 (SinOP? 内? 電信?  50MHz? HBAND?)))
(section JA1 (cat 内 個 電電  50MHz) SinHB (個検査 (SinOP? 内? 電電?  50MHz? HBAND?)))
(section JA1 (cat 内 個 電信 HBANDS) SinHB (個検査 (SinOP? 内? 電信?         HBAND?)))
(section JA1 (cat 内 個 電電 HBANDS) SinHB (個検査 (SinOP? 内? 電電?         HBAND?)))

; 1エリア外 個人 ハイバンド部門
(section JA1 (cat 外 個 電信  14MHz) SinHB (個検査 (SinOP? 外? 電信?  14MHz? HBAND?)))
(section JA1 (cat 外 個 電電  14MHz) SinHB (個検査 (SinOP? 外? 電電?  14MHz? HBAND?)))
(section JA1 (cat 外 個 電信  21MHz) SinHB (個検査 (SinOP? 外? 電信?  21MHz? HBAND?)))
(section JA1 (cat 外 個 電電  21MHz) SinHB (個検査 (SinOP? 外? 電電?  21MHz? HBAND?)))
(section JA1 (cat 外 個 電信  28MHz) SinHB (個検査 (SinOP? 外? 電信?  28MHz? HBAND?)))
(section JA1 (cat 外 個 電電  28MHz) SinHB (個検査 (SinOP? 外? 電電?  28MHz? HBAND?)))
(section JA1 (cat 外 個 電信  50MHz) SinHB (個検査 (SinOP? 外? 電信?  50MHz? HBAND?)))
(section JA1 (cat 外 個 電電  50MHz) SinHB (個検査 (SinOP? 外? 電電?  50MHz? HBAND?)))
(section JA1 (cat 外 個 電信 HBANDS) SinHB (個検査 (SinOP? 外? 電信?         HBAND?)))
(section JA1 (cat 外 個 電電 HBANDS) SinHB (個検査 (SinOP? 外? 電電?         HBAND?)))

; 団体 アナログ部門
(section JA1 (cat 内 団 電信 "部門") MulAB (団検査 (MulOp? 内? 電信?         ABAND?)))
(section JA1 (cat 内 団 電電 "部門") MulAB (団検査 (MulOp? 内? 電電?         ABAND?)))
(section JA1 (cat 外 団 電信 "部門") MulAB (団検査 (MulOp? 外? 電信?         ABAND?)))
(section JA1 (cat 外 団 電電 "部門") MulAB (団検査 (MulOp? 外? 電電?         ABAND?)))

; デジタル部門
(section JA1 (cat 内 個 離散 "部門") SinDG (個検査 (SinOP? 内? 離散?         DIGIT?)))
(section JA1 (cat 外 個 離散 "部門") SinDG (個検査 (SinOP? 外? 離散?         DIGIT?)))
(section JA1 (cat 内 団 離散 "部門") MulDG (団検査 (MulOp? 内? 離散?         DIGIT?)))
(section JA1 (cat 外 団 離散 "部門") MulDG (団検査 (MulOp? 外? 離散?         DIGIT?)))

; 総合部門
(section JA1 (cat 内 個 総合 "部門") SinJS (個検査 (SinOP? 内? 総合?         JOINT?)))
(section JA1 (cat 外 個 総合 "部門") SinJS (個検査 (SinOP? 外? 総合?         JOINT?)))
(section JA1 (cat 内 団 総合 "部門") MulJS (団検査 (MulOp? 内? 総合?         JOINT?)))
(section JA1 (cat 外 団 総合 "部門") MulJS (団検査 (MulOp? 外? 総合?         JOINT?)))
