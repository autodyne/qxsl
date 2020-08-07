;; ALLJA1 CONTEST DEFINED by 無線部開発班

(load "qxsl/ruler/radial.lisp")

; UTC-JST conversion for ADIF
(defun 時刻 it (hour (qxsl-time it) "Asia/Tokyo"))

; mode validation
(defun 電信? it (Morse? it))
(defun 電話? it (Phone? it))
(defun 離散? it (Digit? it))
(defun 電電? it (or (電信? it) (電話? it)))
(defun 総合? it (or (電電? it) (離散? it)))

; multiple-band validation
(defun 高? it (<= 14000 (qxsl-band it) 50000))
(defun 低? it (<=  1900 (qxsl-band it)  7000))

; time validation
(defun 朝? it (and (<= 09 (時刻 it) 11) (電電? it) (高? it)))
(defun 昼? it (and (<= 13 (時刻 it) 14) (離散? it) (7MHz? it)))
(defun 夜? it (and (<= 16 (時刻 it) 19) (電電? it) (低? it)))

; validation of analog/digital sections
(defun 全? it (or (朝? it) (夜? it)))
(defun 総? it (or (朝? it) (夜? it) (昼? it)))

; conversion of JCC/JCG to city/prefecture name
(defun 県 it (city "jarl" (peel-code it) 0))
(defun 市 it (city "jarl" (peel-code it) nil))
(defun 総 it (city "area" (県 it) 2))

; JCC/JCG validation
(defun 現存? it (not (null? (市 it))))
(defun 支庁? it (match (peel-code it) "\\d{3,3}"))
(defun 府県? it (match (peel-code it) "\\d{2,2}"))
(defun 市郡? it (match (peel-code it) "\\d{4,9}"))

(defun 関東? it (equal (総 it) "関東"))
(defun 北海? it (equal (県 it) "北海道"))
(defun 管外? it (not (or (関東? it) (北海? it))))

;; validation of 1エリア内/1エリア外部門
(defun 内? it
	(and
		(現存? it)
		(cond
			((離散? it) (市郡? it))
			((北海? it) (支庁? it))
			((関東? it) (市郡? it))
			((管外? it) (府県? it)))))

(defun 外? it (and (現存? it) (関東? it) (市郡? it)))

;; validation of 個人部門/団体部門
(defun SinOP? it #t)
(defun MulOp? it (not (member (qxsl-name it) (list null ""))))

; keys for scoring
(defun CALL it
	(list
		(qxsl-call it)
		(qxsl-band it)
		(cond
			((電信? it) 1)
			((電話? it) 2)
			((離散? it) 3))))
(defun MULT it
	(list
		(qxsl-band it)
		(peel-code it)))
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
		(block
			(toQXSL it)
			(setq error (search it ,conds))
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
(setq LB部門    "1.9/3.5/7MHz部門")
(setq HB部門  "14/21/28/50MHz部門")

; section name concatenation
(defmacro cat (area opnum mode band) `(concatenate " " (list ,area ,opnum ,mode ,band)))

; contest definition
(CONTEST JA1 "ALLJA1 TEST" 得点)

; 1エリア内 個人 ローバンド部門
(SECTION JA1 (cat 内 個 電信 19部門) SinLB (個検査 (SinOP? 内? 電信? 1.9MHz? 夜?)))
(SECTION JA1 (cat 内 個 電信 35部門) SinLB (個検査 (SinOP? 内? 電信? 3.5MHz? 夜?)))
(SECTION JA1 (cat 内 個 電電 35部門) SinLB (個検査 (SinOP? 内? 電電? 3.5MHz? 夜?)))
(SECTION JA1 (cat 内 個 電信  7部門) SinLB (個検査 (SinOP? 内? 電信?   7MHz? 夜?)))
(SECTION JA1 (cat 内 個 電電  7部門) SinLB (個検査 (SinOP? 内? 電電?   7MHz? 夜?)))
(SECTION JA1 (cat 内 個 電信 LB部門) SinLB (個検査 (SinOP? 内? 電信?         夜?)))
(SECTION JA1 (cat 内 個 電電 LB部門) SinLB (個検査 (SinOP? 内? 電電?         夜?)))

; 1エリア外 個人 ローバンド部門
(SECTION JA1 (cat 外 個 電信 19部門) SinLB (個検査 (SinOP? 外? 電信? 1.9MHz? 夜?)))
(SECTION JA1 (cat 外 個 電信 35部門) SinLB (個検査 (SinOP? 外? 電信? 3.5MHz? 夜?)))
(SECTION JA1 (cat 外 個 電電 35部門) SinLB (個検査 (SinOP? 外? 電電? 3.5MHz? 夜?)))
(SECTION JA1 (cat 外 個 電信  7部門) SinLB (個検査 (SinOP? 外? 電信?   7MHz? 夜?)))
(SECTION JA1 (cat 外 個 電電  7部門) SinLB (個検査 (SinOP? 外? 電電?   7MHz? 夜?)))
(SECTION JA1 (cat 外 個 電信 LB部門) SinLB (個検査 (SinOP? 外? 電信?         夜?)))
(SECTION JA1 (cat 外 個 電電 LB部門) SinLB (個検査 (SinOP? 外? 電電?         夜?)))

; 1エリア内 個人 ハイバンド部門
(SECTION JA1 (cat 内 個 電信 14部門) SinHB (個検査 (SinOP? 内? 電信?  14MHz? 朝?)))
(SECTION JA1 (cat 内 個 電電 14部門) SinHB (個検査 (SinOP? 内? 電電?  14MHz? 朝?)))
(SECTION JA1 (cat 内 個 電信 21部門) SinHB (個検査 (SinOP? 内? 電信?  21MHz? 朝?)))
(SECTION JA1 (cat 内 個 電電 21部門) SinHB (個検査 (SinOP? 内? 電電?  21MHz? 朝?)))
(SECTION JA1 (cat 内 個 電信 28部門) SinHB (個検査 (SinOP? 内? 電信?  28MHz? 朝?)))
(SECTION JA1 (cat 内 個 電電 28部門) SinHB (個検査 (SinOP? 内? 電電?  28MHz? 朝?)))
(SECTION JA1 (cat 内 個 電信 50部門) SinHB (個検査 (SinOP? 内? 電信?  50MHz? 朝?)))
(SECTION JA1 (cat 内 個 電電 50部門) SinHB (個検査 (SinOP? 内? 電電?  50MHz? 朝?)))
(SECTION JA1 (cat 内 個 電信 HB部門) SinHB (個検査 (SinOP? 内? 電信?         朝?)))
(SECTION JA1 (cat 内 個 電電 HB部門) SinHB (個検査 (SinOP? 内? 電電?         朝?)))

; 1エリア外 個人 ハイバンド部門
(SECTION JA1 (cat 外 個 電信 14部門) SinHB (個検査 (SinOP? 外? 電信?  14MHz? 朝?)))
(SECTION JA1 (cat 外 個 電電 14部門) SinHB (個検査 (SinOP? 外? 電電?  14MHz? 朝?)))
(SECTION JA1 (cat 外 個 電信 21部門) SinHB (個検査 (SinOP? 外? 電信?  21MHz? 朝?)))
(SECTION JA1 (cat 外 個 電電 21部門) SinHB (個検査 (SinOP? 外? 電電?  21MHz? 朝?)))
(SECTION JA1 (cat 外 個 電信 28部門) SinHB (個検査 (SinOP? 外? 電信?  28MHz? 朝?)))
(SECTION JA1 (cat 外 個 電電 28部門) SinHB (個検査 (SinOP? 外? 電電?  28MHz? 朝?)))
(SECTION JA1 (cat 外 個 電信 50部門) SinHB (個検査 (SinOP? 外? 電信?  50MHz? 朝?)))
(SECTION JA1 (cat 外 個 電電 50部門) SinHB (個検査 (SinOP? 外? 電電?  50MHz? 朝?)))
(SECTION JA1 (cat 外 個 電信 HB部門) SinHB (個検査 (SinOP? 外? 電信?         朝?)))
(SECTION JA1 (cat 外 個 電電 HB部門) SinHB (個検査 (SinOP? 外? 電電?         朝?)))

; 団体 アナログ部門
(SECTION JA1 (cat 内 団 電信 "部門") MulAB (団検査 (MulOp? 内? 電信?         全?)))
(SECTION JA1 (cat 内 団 電電 "部門") MulAB (団検査 (MulOp? 内? 電電?         全?)))
(SECTION JA1 (cat 外 団 電信 "部門") MulAB (団検査 (MulOp? 外? 電信?         全?)))
(SECTION JA1 (cat 外 団 電電 "部門") MulAB (団検査 (MulOp? 外? 電電?         全?)))

; デジタル部門
(SECTION JA1 (cat 内 個 離散 "部門") SinDG (個検査 (SinOP? 内? 離散?         昼?)))
(SECTION JA1 (cat 外 個 離散 "部門") SinDG (個検査 (SinOP? 外? 離散?         昼?)))
(SECTION JA1 (cat 内 団 離散 "部門") MulDG (団検査 (MulOp? 内? 離散?         昼?)))
(SECTION JA1 (cat 外 団 離散 "部門") MulDG (団検査 (MulOp? 外? 離散?         昼?)))

; 総合部門
(SECTION JA1 (cat 内 個 総合 "部門") SinJS (個検査 (SinOP? 内? 総合?         総?)))
(SECTION JA1 (cat 外 個 総合 "部門") SinJS (個検査 (SinOP? 外? 総合?         総?)))
(SECTION JA1 (cat 内 団 総合 "部門") MulJS (団検査 (MulOp? 内? 総合?         総?)))
(SECTION JA1 (cat 外 団 総合 "部門") MulJS (団検査 (MulOp? 外? 総合?         総?)))
