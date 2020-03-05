;; ALLJA1 CONTEST DEFINED by 無線部開発班

(load "format.lisp")

; UTC-JST conversion for ADIF
(defun HOUR it (hour (qxsl-time it) "JST"))

; mode validation
(defun 電信? it (match "(?i)CW" (qxsl-mode it)))
(defun 電話? it (match "(?i)PH|AM|FM|[DS]SB" (qxsl-mode it)))
(defun 離散? it (match "(?i)DG|FT4|FT8|RTTY" (qxsl-mode it)))
(defun 電電? it (or (電信? it) (電話? it)))

; time validation
(defun 朝? it (<= 09 (HOUR it) 11))
(defun 昼? it (<= 13 (HOUR it) 14))
(defun 夜? it (<= 16 (HOUR it) 19))

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
(defun SIN-OP? it #t)
(defun MUL-OP? it (not (member (qxsl-name it) (list null ""))))

; keys for scoring
(defun MODES-key it
	(cond (
		((電信? it) 1)
		((電話? it) 2)
		((離散? it) 3))))
(defun CALLS-key it
	(list
		(qxsl-call it)
		(qxsl-band it)
		(MODES-key it)))
(defun MULTS-key it
	(list
		(qxsl-band it)
		(qxsl-JCCG it)))
(defun DUMMY-key it null)
(defun PARTY-key it (qxsl-name it))

; scoring
(defmacro scoring (score calls mults names)
	`(let (
		(mults (length (quote ,mults)))
		(names (length (quote ,names))))
		(ceiling (/ (* ,score mults) names))))

; validation routine
(defmacro verify (conds keys)
	`(lambda it
		(progn
			(toQXSL it)
			(setq error (unsat it ,conds))
			(if
				(nil? error)
				(success it 1 ,@(dolist (k keys) (list k 'it)))
				(failure it (format "無効な交信 %s" error))))))

(defmacro 個 conds `(verify ,conds (CALLS-key MULTS-key DUMMY-key)))
(defmacro 団 conds `(verify ,conds (CALLS-key MULTS-key PARTY-key)))

; section codes
(setq SinHB "Sin1")
(setq SinLB "Sin2")
(setq SinDG "Sin3")
(setq SinJS "Sin4")
(setq MulAB "Mul1")
(setq MulDG "Mul2")
(setq MulJS "Mul3")

(contest "ALLJA1 TEST" scoring
	(section "1エリア内 個人 電信限定 1.9MHz部門"         SinLB (個 (SIN-OP? 内? 電信? 1.9MHz? LBAND?)))
	(section "1エリア内 個人 電信限定 3.5MHz部門"         SinLB (個 (SIN-OP? 内? 電信? 3.5MHz? LBAND?)))
	(section "1エリア内 個人 電信電話 3.5MHz部門"         SinLB (個 (SIN-OP? 内?       3.5MHz? LBAND?)))
	(section "1エリア内 個人 電信限定 7MHz部門"           SinLB (個 (SIN-OP? 内? 電信?   7MHz? LBAND?)))
	(section "1エリア内 個人 電信電話 7MHz部門"           SinLB (個 (SIN-OP? 内?         7MHz? LBAND?)))
	(section "1エリア内 個人 電信限定 1.9/3.5/7MHz部門"   SinLB (個 (SIN-OP? 内? 電信?         LBAND?)))
	(section "1エリア内 個人 電信電話 1.9/3.5/7MHz部門"   SinLB (個 (SIN-OP? 内?               LBAND?)))

	(section "1エリア外 個人 電信限定 1.9MHz部門"         SinLB (個 (SIN-OP? 外? 電信? 1.9MHz? LBAND?)))
	(section "1エリア外 個人 電信限定 3.5MHz部門"         SinLB (個 (SIN-OP? 外? 電信? 3.5MHz? LBAND?)))
	(section "1エリア外 個人 電信電話 3.5MHz部門"         SinLB (個 (SIN-OP? 外?       3.5MHz? LBAND?)))
	(section "1エリア外 個人 電信限定 7MHz部門"           SinLB (個 (SIN-OP? 外? 電信?   7MHz? LBAND?)))
	(section "1エリア外 個人 電信電話 7MHz部門"           SinLB (個 (SIN-OP? 外?         7MHz? LBAND?)))
	(section "1エリア外 個人 電信限定 1.9/3.5/7MHz部門"   SinLB (個 (SIN-OP? 外? 電信?         LBAND?)))
	(section "1エリア外 個人 電信電話 1.9/3.5/7MHz部門"   SinLB (個 (SIN-OP? 外?               LBAND?)))

	(section "1エリア内 個人 電信限定 14MHz部門"          SinHB (個 (SIN-OP? 内? 電信?  14MHz? HBAND?)))
	(section "1エリア内 個人 電信電話 14MHz部門"          SinHB (個 (SIN-OP? 内?        14MHz? HBAND?)))
	(section "1エリア内 個人 電信限定 21MHz部門"          SinHB (個 (SIN-OP? 内? 電信?  21MHz? HBAND?)))
	(section "1エリア内 個人 電信電話 21MHz部門"          SinHB (個 (SIN-OP? 内?        21MHz? HBAND?)))
	(section "1エリア内 個人 電信限定 28MHz部門"          SinHB (個 (SIN-OP? 内? 電信?  28MHz? HBAND?)))
	(section "1エリア内 個人 電信電話 28MHz部門"          SinHB (個 (SIN-OP? 内?        28MHz? HBAND?)))
	(section "1エリア内 個人 電信限定 50MHz部門"          SinHB (個 (SIN-OP? 内? 電信?  50MHz? HBAND?)))
	(section "1エリア内 個人 電信電話 50MHz部門"          SinHB (個 (SIN-OP? 内?        50MHz? HBAND?)))
	(section "1エリア内 個人 電信限定 14/21/28/50MHz部門" SinHB (個 (SIN-OP? 内? 電信?         HBAND?)))
	(section "1エリア内 個人 電信電話 14/21/28/50MHz部門" SinHB (個 (SIN-OP? 内?               HBAND?)))

	(section "1エリア外 個人 電信限定 14MHz部門"          SinHB (個 (SIN-OP? 外? 電信?  14MHz? HBAND?)))
	(section "1エリア外 個人 電信電話 14MHz部門"          SinHB (個 (SIN-OP? 外?        14MHz? HBAND?)))
	(section "1エリア外 個人 電信限定 21MHz部門"          SinHB (個 (SIN-OP? 外? 電信?  21MHz? HBAND?)))
	(section "1エリア外 個人 電信電話 21MHz部門"          SinHB (個 (SIN-OP? 外?        21MHz? HBAND?)))
	(section "1エリア外 個人 電信限定 28MHz部門"          SinHB (個 (SIN-OP? 外? 電信?  28MHz? HBAND?)))
	(section "1エリア外 個人 電信電話 28MHz部門"          SinHB (個 (SIN-OP? 外?        28MHz? HBAND?)))
	(section "1エリア外 個人 電信限定 50MHz部門"          SinHB (個 (SIN-OP? 外? 電信?  50MHz? HBAND?)))
	(section "1エリア外 個人 電信電話 50MHz部門"          SinHB (個 (SIN-OP? 外?        50MHz? HBAND?)))
	(section "1エリア外 個人 電信限定 14/21/28/50MHz部門" SinHB (個 (SIN-OP? 外? 電信?         HBAND?)))
	(section "1エリア外 個人 電信電話 14/21/28/50MHz部門" SinHB (個 (SIN-OP? 外?               HBAND?)))

	(section "1エリア内 団体 電信限定 部門"               MulAB (団 (MUL-OP? 内? 電信?         ABAND?)))
	(section "1エリア内 団体 電信電話 部門"               MulAB (団 (MUL-OP? 内?               ABAND?)))
	(section "1エリア外 団体 電信限定 部門"               MulAB (団 (MUL-OP? 外? 電信?         ABAND?)))
	(section "1エリア外 団体 電信電話 部門"               MulAB (団 (MUL-OP? 外?               ABAND?)))

	(section "1エリア内 個人 デジタル 部門"               SinDG (個 (SIN-OP? 内?               DIGIT?)))
	(section "1エリア外 個人 デジタル 部門"               SinDG (個 (SIN-OP? 外?               DIGIT?)))
	(section "1エリア内 団体 デジタル 部門"               MulDG (団 (MUL-OP? 内?               DIGIT?)))
	(section "1エリア外 団体 デジタル 部門"               MulDG (団 (MUL-OP? 外?               DIGIT?)))

	(section "1エリア内 個人 総合 部門"                   SinJS (個 (SIN-OP? 内?               JOINT?)))
	(section "1エリア外 個人 総合 部門"                   SinJS (個 (SIN-OP? 外?               JOINT?)))
	(section "1エリア内 団体 総合 部門"                   MulJS (団 (MUL-OP? 内?               JOINT?)))
	(section "1エリア外 団体 総合 部門"                   MulJS (団 (MUL-OP? 外?               JOINT?))))
