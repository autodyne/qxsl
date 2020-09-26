;; ALLJA1 CONTEST DEFINED by 無線部開発班

(load "qxsl/ruler/format.lisp")

; UTC-JST conversion for ADIF
(defun 時刻 it (hour (at-zone (qxsl-time it) (zone "Asia/Tokyo"))))

; mode validation
(defun cw/ph? it (or (morse? it) (phone? it)))
(defun an/dg? it (or (cw/ph? it) (digit? it)))

; multiple-band validation
(defun 1450? it (<= 14000 (qxsl-band it) 50000))
(defun 1970? it (<=  1900 (qxsl-band it)  7000))

; time validation
(defun 朝の部門? it (and (<= 09 (時刻 it) 11) (cw/ph? it) (1450? it)))
(defun 昼の部門? it (and (<= 13 (時刻 it) 14) (digit? it) (7MHz? it)))
(defun 夜の部門? it (and (<= 16 (時刻 it) 19) (cw/ph? it) (1970? it)))

; time-band validation
(defun 1.9M部門? it (every it 夜の部門? 1.9MHz?))
(defun 3.5M部門? it (every it 夜の部門? 3.5MHz?))
(defun   7M部門? it (every it 夜の部門?   7MHz?))
(defun  14M部門? it (every it 朝の部門?  14MHz?))
(defun  21M部門? it (every it 朝の部門?  21MHz?))
(defun  28M部門? it (every it 朝の部門?  28MHz?))
(defun  50M部門? it (every it 朝の部門?  50MHz?))

; validation of analog/digital sections
(defun 電信電話? it (some it 朝の部門? 夜の部門? ))
(defun 総合部門? it (some it 朝の部門? 夜の部門? 昼の部門?))

; conversion of JCC/JCG to city/prefecture name
(defun 市区 it (city "jarl" (qxsl-code it)))
(defun 県名 it (city-region (市区 it) 0))
(defun 総通 it (city-region (city "area" (県名 it)) 2))

; JCC/JCG validation
(defun 現存? it (not (null? (市区 it))))
(defun 支庁? it (match (qxsl-code it) "\\d{3,3}"))
(defun 府県? it (match (qxsl-code it) "\\d{2,2}"))
(defun 市郡? it (match (qxsl-code it) "\\d{4,9}"))

(defun Area1? it (equal (総通 it) "関東"))
(defun Area8? it (equal (県名 it) "北海道"))
(defun other? it #t)

;; validation of 1エリア内/1エリア外部門
(defun 関東内? it
	(and
		(現存? it)
		(cond
			((digit? it) (市郡? it))
			((Area1? it) (市郡? it))
			((Area8? it) (支庁? it))
			((other? it) (府県? it)))))

(defun 関東外? it (every it 現存? Area1? 市郡?))

;; validation of 個人部門/団体部門
(defun SinOp? it #t)
(defun MulOp? it (not (member (qxsl-name it) (list null ""))))

; keys for scoring
(defun CALL it
	(list
		(qxsl-call it)
		(qxsl-band it)
		(cond
			((morse? it) 1)
			((phone? it) 2)
			((digit? it) 3))))
(defun MULT it
	(list
		(qxsl-band it)
		(qxsl-code it)))
(defun SOLE it null)
(defun CORP it (qxsl-name it))

; scoring
(defmacro 得点 (score calls mults names)
	`(block
		(setq mults (length (quote ,mults)))
		(setq names (length (quote ,names)))
		(ceiling (/ (* ,score mults) names))))

; validation routine
(defmacro 検査 (conds keys)
	`(lambda it
		(let it (decode it)
			(let msg (search it ,conds)
				(if (nil? msg)
					(success it 1 ,@(dolist (k keys) (list k 'it)))
					(failure it (format "無効な交信(%s)" msg)))))))

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

; section names
(setq 内 "1エリア内")
(setq 外 "1エリア外")

(setq 個 "個人")
(setq 団 "団体")

(setq 電信 "電信限定")
(setq 電電 "電信電話")
(setq 離散 "デジタル")
(setq 総合 "総合")

(setq 19部門 "1.9MHz部門")
(setq 35部門 "3.5MHz部門")
(setq  7部門   "7MHz部門")
(setq 14部門  "14MHz部門")
(setq 21部門  "21MHz部門")
(setq 28部門  "28MHz部門")
(setq 50部門  "50MHz部門")
(setq LB部門 "1.9-7MHz部門")
(setq HB部門 "14-50MHz部門")

; section name concatenation
(defmacro cat (area op mode band) `(format "%s %s %s %s" ,area ,op ,mode ,band))

; contest definition
(set-contest JA1 "ALLJA1 TEST")

; 1エリア内 個人 ローバンド部門
(add-section JA1 (cat 内 個 電信 19部門) SinLB (個検査 (SinOp? 関東内? morse? 1.9M部門?)) 得点)
(add-section JA1 (cat 内 個 電信 35部門) SinLB (個検査 (SinOp? 関東内? morse? 3.5M部門?)) 得点)
(add-section JA1 (cat 内 個 電電 35部門) SinLB (個検査 (SinOp? 関東内? cw/ph? 3.5M部門?)) 得点)
(add-section JA1 (cat 内 個 電信  7部門) SinLB (個検査 (SinOp? 関東内? morse?   7M部門?)) 得点)
(add-section JA1 (cat 内 個 電電  7部門) SinLB (個検査 (SinOp? 関東内? cw/ph?   7M部門?)) 得点)
(add-section JA1 (cat 内 個 電信 LB部門) SinLB (個検査 (SinOp? 関東内? morse? 夜の部門?)) 得点)
(add-section JA1 (cat 内 個 電電 LB部門) SinLB (個検査 (SinOp? 関東内? cw/ph? 夜の部門?)) 得点)

; 1エリア外 個人 ローバンド部門
(add-section JA1 (cat 外 個 電信 19部門) SinLB (個検査 (SinOp? 関東外? morse? 1.9M部門?)) 得点)
(add-section JA1 (cat 外 個 電信 35部門) SinLB (個検査 (SinOp? 関東外? morse? 3.5M部門?)) 得点)
(add-section JA1 (cat 外 個 電電 35部門) SinLB (個検査 (SinOp? 関東外? cw/ph? 3.5M部門?)) 得点)
(add-section JA1 (cat 外 個 電信  7部門) SinLB (個検査 (SinOp? 関東外? morse?   7M部門?)) 得点)
(add-section JA1 (cat 外 個 電電  7部門) SinLB (個検査 (SinOp? 関東外? cw/ph?   7M部門?)) 得点)
(add-section JA1 (cat 外 個 電信 LB部門) SinLB (個検査 (SinOp? 関東外? morse? 夜の部門?)) 得点)
(add-section JA1 (cat 外 個 電電 LB部門) SinLB (個検査 (SinOp? 関東外? cw/ph? 夜の部門?)) 得点)

; 1エリア内 個人 ハイバンド部門
(add-section JA1 (cat 内 個 電信 14部門) SinHB (個検査 (SinOp? 関東内? morse?  14M部門?)) 得点)
(add-section JA1 (cat 内 個 電電 14部門) SinHB (個検査 (SinOp? 関東内? cw/ph?  14M部門?)) 得点)
(add-section JA1 (cat 内 個 電信 21部門) SinHB (個検査 (SinOp? 関東内? morse?  21M部門?)) 得点)
(add-section JA1 (cat 内 個 電電 21部門) SinHB (個検査 (SinOp? 関東内? cw/ph?  21M部門?)) 得点)
(add-section JA1 (cat 内 個 電信 28部門) SinHB (個検査 (SinOp? 関東内? morse?  28M部門?)) 得点)
(add-section JA1 (cat 内 個 電電 28部門) SinHB (個検査 (SinOp? 関東内? cw/ph?  28M部門?)) 得点)
(add-section JA1 (cat 内 個 電信 50部門) SinHB (個検査 (SinOp? 関東内? morse?  50M部門?)) 得点)
(add-section JA1 (cat 内 個 電電 50部門) SinHB (個検査 (SinOp? 関東内? cw/ph?  50M部門?)) 得点)
(add-section JA1 (cat 内 個 電信 HB部門) SinHB (個検査 (SinOp? 関東内? morse? 朝の部門?)) 得点)
(add-section JA1 (cat 内 個 電電 HB部門) SinHB (個検査 (SinOp? 関東内? cw/ph? 朝の部門?)) 得点)

; 1エリア外 個人 ハイバンド部門
(add-section JA1 (cat 外 個 電信 14部門) SinHB (個検査 (SinOp? 関東外? morse?  14M部門?)) 得点)
(add-section JA1 (cat 外 個 電電 14部門) SinHB (個検査 (SinOp? 関東外? cw/ph?  14M部門?)) 得点)
(add-section JA1 (cat 外 個 電信 21部門) SinHB (個検査 (SinOp? 関東外? morse?  21M部門?)) 得点)
(add-section JA1 (cat 外 個 電電 21部門) SinHB (個検査 (SinOp? 関東外? cw/ph?  21M部門?)) 得点)
(add-section JA1 (cat 外 個 電信 28部門) SinHB (個検査 (SinOp? 関東外? morse?  28M部門?)) 得点)
(add-section JA1 (cat 外 個 電電 28部門) SinHB (個検査 (SinOp? 関東外? cw/ph?  28M部門?)) 得点)
(add-section JA1 (cat 外 個 電信 50部門) SinHB (個検査 (SinOp? 関東外? morse?  50M部門?)) 得点)
(add-section JA1 (cat 外 個 電電 50部門) SinHB (個検査 (SinOp? 関東外? cw/ph?  50M部門?)) 得点)
(add-section JA1 (cat 外 個 電信 HB部門) SinHB (個検査 (SinOp? 関東外? morse? 朝の部門?)) 得点)
(add-section JA1 (cat 外 個 電電 HB部門) SinHB (個検査 (SinOp? 関東外? cw/ph? 朝の部門?)) 得点)

; 団体 アナログ部門
(add-section JA1 (cat 内 団 電信 "部門") MulAB (団検査 (MulOp? 関東内? morse? 電信電話?)) 得点)
(add-section JA1 (cat 内 団 電電 "部門") MulAB (団検査 (MulOp? 関東内? cw/ph? 電信電話?)) 得点)
(add-section JA1 (cat 外 団 電信 "部門") MulAB (団検査 (MulOp? 関東外? morse? 電信電話?)) 得点)
(add-section JA1 (cat 外 団 電電 "部門") MulAB (団検査 (MulOp? 関東外? cw/ph? 電信電話?)) 得点)

; デジタル部門
(add-section JA1 (cat 内 個 離散 "部門") SinDG (個検査 (SinOp? 関東内? digit? 昼の部門?)) 得点)
(add-section JA1 (cat 外 個 離散 "部門") SinDG (個検査 (SinOp? 関東外? digit? 昼の部門?)) 得点)
(add-section JA1 (cat 内 団 離散 "部門") MulDG (団検査 (MulOp? 関東内? digit? 昼の部門?)) 得点)
(add-section JA1 (cat 外 団 離散 "部門") MulDG (団検査 (MulOp? 関東外? digit? 昼の部門?)) 得点)

; 総合部門
(add-section JA1 (cat 内 個 総合 "部門") SinJS (個検査 (SinOp? 関東内? an/dg? 総合部門?)) 得点)
(add-section JA1 (cat 外 個 総合 "部門") SinJS (個検査 (SinOp? 関東外? an/dg? 総合部門?)) 得点)
(add-section JA1 (cat 内 団 総合 "部門") MulJS (団検査 (MulOp? 関東内? an/dg? 総合部門?)) 得点)
(add-section JA1 (cat 外 団 総合 "部門") MulJS (団検査 (MulOp? 関東外? an/dg? 総合部門?)) 得点)
