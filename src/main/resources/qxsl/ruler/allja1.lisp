;; ALLJA1 CONTEST DEFINED by 無線部開発班

(load "qxsl/ruler/jautil.lisp")

; time validation
(defun 朝の部門? it (and (<= 09 (時刻 it) 11) (CW/PH? it) (1450? it)))
(defun 昼の部門? it (and (<= 13 (時刻 it) 14) (DIGIT? it) (7MHz? it)))
(defun 夜の部門? it (and (<= 16 (時刻 it) 19) (CW/PH? it) (1970? it)))

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

;; validation of 1エリア内/1エリア外部門
(defun エリア内? it
	(and
		(現存? it)
		(cond
			((DIGIT? it) (市郡? it))
			((AREA1? it) (市郡? it))
			((AREA8? it) (支庁? it))
			((always it) (府県? it)))))

(defun エリア外? it (every it 現存? AREA1? 市郡?))

;; validation of 個人部門/団体部門
(defun SinOp? it #t)
(defun MulOp? it (not (member (qxsl-name it) (list null ""))))

; keys for scoring
(defun CALL it
	(list
		(qxsl-call it)
		(qxsl-band it)
		(cond
			((MORSE? it) 1)
			((PHONE? it) 2)
			((DIGIT? it) 3))))
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
(set-contest JA1 "ALLJA1" "東大無線部" "allja1@ja1zlo.u-tokyo.org" "ja1zlo.u-tokyo.org")

; 1エリア内 個人 ローバンド部門
(add-section JA1 (cat 内 個 電信 19部門) SinLB (個検査 (SinOp? エリア内? MORSE? 1.9M部門?)) 得点)
(add-section JA1 (cat 内 個 電信 35部門) SinLB (個検査 (SinOp? エリア内? MORSE? 3.5M部門?)) 得点)
(add-section JA1 (cat 内 個 電電 35部門) SinLB (個検査 (SinOp? エリア内? CW/PH? 3.5M部門?)) 得点)
(add-section JA1 (cat 内 個 電信  7部門) SinLB (個検査 (SinOp? エリア内? MORSE?   7M部門?)) 得点)
(add-section JA1 (cat 内 個 電電  7部門) SinLB (個検査 (SinOp? エリア内? CW/PH?   7M部門?)) 得点)
(add-section JA1 (cat 内 個 電信 LB部門) SinLB (個検査 (SinOp? エリア内? MORSE? 夜の部門?)) 得点)
(add-section JA1 (cat 内 個 電電 LB部門) SinLB (個検査 (SinOp? エリア内? CW/PH? 夜の部門?)) 得点)

; 1エリア外 個人 ローバンド部門
(add-section JA1 (cat 外 個 電信 19部門) SinLB (個検査 (SinOp? エリア外? MORSE? 1.9M部門?)) 得点)
(add-section JA1 (cat 外 個 電信 35部門) SinLB (個検査 (SinOp? エリア外? MORSE? 3.5M部門?)) 得点)
(add-section JA1 (cat 外 個 電電 35部門) SinLB (個検査 (SinOp? エリア外? CW/PH? 3.5M部門?)) 得点)
(add-section JA1 (cat 外 個 電信  7部門) SinLB (個検査 (SinOp? エリア外? MORSE?   7M部門?)) 得点)
(add-section JA1 (cat 外 個 電電  7部門) SinLB (個検査 (SinOp? エリア外? CW/PH?   7M部門?)) 得点)
(add-section JA1 (cat 外 個 電信 LB部門) SinLB (個検査 (SinOp? エリア外? MORSE? 夜の部門?)) 得点)
(add-section JA1 (cat 外 個 電電 LB部門) SinLB (個検査 (SinOp? エリア外? CW/PH? 夜の部門?)) 得点)

; 1エリア内 個人 ハイバンド部門
(add-section JA1 (cat 内 個 電信 14部門) SinHB (個検査 (SinOp? エリア内? MORSE?  14M部門?)) 得点)
(add-section JA1 (cat 内 個 電電 14部門) SinHB (個検査 (SinOp? エリア内? CW/PH?  14M部門?)) 得点)
(add-section JA1 (cat 内 個 電信 21部門) SinHB (個検査 (SinOp? エリア内? MORSE?  21M部門?)) 得点)
(add-section JA1 (cat 内 個 電電 21部門) SinHB (個検査 (SinOp? エリア内? CW/PH?  21M部門?)) 得点)
(add-section JA1 (cat 内 個 電信 28部門) SinHB (個検査 (SinOp? エリア内? MORSE?  28M部門?)) 得点)
(add-section JA1 (cat 内 個 電電 28部門) SinHB (個検査 (SinOp? エリア内? CW/PH?  28M部門?)) 得点)
(add-section JA1 (cat 内 個 電信 50部門) SinHB (個検査 (SinOp? エリア内? MORSE?  50M部門?)) 得点)
(add-section JA1 (cat 内 個 電電 50部門) SinHB (個検査 (SinOp? エリア内? CW/PH?  50M部門?)) 得点)
(add-section JA1 (cat 内 個 電信 HB部門) SinHB (個検査 (SinOp? エリア内? MORSE? 朝の部門?)) 得点)
(add-section JA1 (cat 内 個 電電 HB部門) SinHB (個検査 (SinOp? エリア内? CW/PH? 朝の部門?)) 得点)

; 1エリア外 個人 ハイバンド部門
(add-section JA1 (cat 外 個 電信 14部門) SinHB (個検査 (SinOp? エリア外? MORSE?  14M部門?)) 得点)
(add-section JA1 (cat 外 個 電電 14部門) SinHB (個検査 (SinOp? エリア外? CW/PH?  14M部門?)) 得点)
(add-section JA1 (cat 外 個 電信 21部門) SinHB (個検査 (SinOp? エリア外? MORSE?  21M部門?)) 得点)
(add-section JA1 (cat 外 個 電電 21部門) SinHB (個検査 (SinOp? エリア外? CW/PH?  21M部門?)) 得点)
(add-section JA1 (cat 外 個 電信 28部門) SinHB (個検査 (SinOp? エリア外? MORSE?  28M部門?)) 得点)
(add-section JA1 (cat 外 個 電電 28部門) SinHB (個検査 (SinOp? エリア外? CW/PH?  28M部門?)) 得点)
(add-section JA1 (cat 外 個 電信 50部門) SinHB (個検査 (SinOp? エリア外? MORSE?  50M部門?)) 得点)
(add-section JA1 (cat 外 個 電電 50部門) SinHB (個検査 (SinOp? エリア外? CW/PH?  50M部門?)) 得点)
(add-section JA1 (cat 外 個 電信 HB部門) SinHB (個検査 (SinOp? エリア外? MORSE? 朝の部門?)) 得点)
(add-section JA1 (cat 外 個 電電 HB部門) SinHB (個検査 (SinOp? エリア外? CW/PH? 朝の部門?)) 得点)

; 団体 アナログ部門
(add-section JA1 (cat 内 団 電信 "部門") MulAB (団検査 (MulOp? エリア内? MORSE? 電信電話?)) 得点)
(add-section JA1 (cat 内 団 電電 "部門") MulAB (団検査 (MulOp? エリア内? CW/PH? 電信電話?)) 得点)
(add-section JA1 (cat 外 団 電信 "部門") MulAB (団検査 (MulOp? エリア外? MORSE? 電信電話?)) 得点)
(add-section JA1 (cat 外 団 電電 "部門") MulAB (団検査 (MulOp? エリア外? CW/PH? 電信電話?)) 得点)

; デジタル部門
(add-section JA1 (cat 内 個 離散 "部門") SinDG (個検査 (SinOp? エリア内? DIGIT? 昼の部門?)) 得点)
(add-section JA1 (cat 外 個 離散 "部門") SinDG (個検査 (SinOp? エリア外? DIGIT? 昼の部門?)) 得点)
(add-section JA1 (cat 内 団 離散 "部門") MulDG (団検査 (MulOp? エリア内? DIGIT? 昼の部門?)) 得点)
(add-section JA1 (cat 外 団 離散 "部門") MulDG (団検査 (MulOp? エリア外? DIGIT? 昼の部門?)) 得点)

; 総合部門
(add-section JA1 (cat 内 個 総合 "部門") SinJS (個検査 (SinOp? エリア内? AN/DG? 総合部門?)) 得点)
(add-section JA1 (cat 外 個 総合 "部門") SinJS (個検査 (SinOp? エリア外? AN/DG? 総合部門?)) 得点)
(add-section JA1 (cat 内 団 総合 "部門") MulJS (団検査 (MulOp? エリア内? AN/DG? 総合部門?)) 得点)
(add-section JA1 (cat 外 団 総合 "部門") MulJS (団検査 (MulOp? エリア外? AN/DG? 総合部門?)) 得点)
