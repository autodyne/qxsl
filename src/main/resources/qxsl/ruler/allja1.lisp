;; ALLJA1 CONTEST DEFINED by 無線部開発班

(load "format.lisp")

; UTC-JST conversion for ADIF
(defun HOUR it (hour (qxsl-time it) "JST"))

; JCC/JCG
(defun peel-JCCG it
	(car
		(cdr
			(tokenize
				(if
					(信? it)
					"^..."
					"^..")
				(qxsl-code it)))))
(defun qxsl-JCCG it
	(if
		(null? (qxsl-rstq it))
		(peel-JCCG it)
		(qxsl-code it)))

; conversion of JCC/JCG to city/prefecture name
(defun 県 it (city "jarl" (qxsl-JCCG it) 0))
(defun 市 it (city "jarl" (qxsl-JCCG it)))
(defun 総 it (city "area" (県 it) 2))

; mode validation
(defun 信? it (match "(?i)CW" (qxsl-mode it)))
(defun 離? it (match "(?i)(DG|FT4|FT8)" (qxsl-mode it)))
(defun 話? it (match "(?i)(PH|AM|FM|SSB|DSB|LSB|USB)" (qxsl-mode it)))
(defun 連? it (or (信? it) (話? it)))

; time validation
(defun HB? it (and (<= 09 (HOUR it) 11) (連? it) (<= 14000 (qxsl-band it) 50000)))
(defun LB? it (and (<= 16 (HOUR it) 19) (連? it) (<=  1900 (qxsl-band it)  7000)))
(defun DB? it (and (<= 13 (HOUR it) 14) (離? it)))
(defun AB? it (or (HB? it) (LB? it)))

; single lower-band validation
(defun 1.9MHz? it (equal (qxsl-band it)  1900))
(defun 3.5MHz? it (equal (qxsl-band it)  3500))
(defun   7MHz? it (equal (qxsl-band it)  7000))

; single UPPER-BAND validation
(defun  14MHz? it (equal (qxsl-band it) 14000))
(defun  21MHz? it (equal (qxsl-band it) 21000))
(defun  28MHz? it (equal (qxsl-band it) 28000))
(defun  50MHz? it (equal (qxsl-band it) 50000))

; JCC/JCG validation
(defun 現存? it (not (null? (市 it))))
(defun 支庁? it (match "\\d{3}"  (qxsl-JCCG it)))
(defun 府県? it (match "\\d{2}"  (qxsl-JCCG it)))
(defun 市郡? it (match "\\d{4,}" (qxsl-JCCG it)))

(defun 関? it (equal (総 it) "関東"))
(defun 道? it (equal (県 it) "北海道"))
(defun 他? it (not (or (関? it) (道? it))))

;; validation of 1エリア内/1エリア外部門
(defun 内? it
	(and
		(現存? it)
		(cond (
			((離? it) (市郡? it))
			((道? it) (支庁? it))
			((関? it) (市郡? it))
			((他? it) (府県? it))))))

(defun 外? it (and (現存? it) (関? it) (市郡? it)))

;; validation of 個人部門/団体部門
(defun 個? it true)
(defun 団? it (not (null? (qxsl-mode it))))

; keys for scoring
(defun qxsl-MODE it
	(cond (
		((信? it) 1)
		((話? it) 2)
		((離? it) 3))))
(defun qxsl-CALL it
	(list
		(qxsl-call it)
		(qxsl-band it)
		(qxsl-MODE it)))
(defun qxsl-MULT it
	(list
		(qxsl-band it)
		(qxsl-JCCG it)))

; scoring
(defmacro scoring (score calls mults names)
	`(progn
		(setq mults (length (quote ,mults)))
		(setq names (length (quote ,names)))
		(/ (* ,score mults) names)))

; validation routine
(defmacro verify conds
	`(lambda it
		(progn
			(toQXSL it)
			(if
				(forall it ,conds)
				(success it 1
					(qxsl-CALL it)
					(qxsl-MULT it)
					(qxsl-name it))
				(failure it "無効な交信")))))

; section codes
(setq HB "アナログ ハイバンド部門")
(setq LB "アナログ ローバンド部門")
(setq DB "デジタル 全周波数帯部門")
(setq AB "アナログ 全周波数帯部門")

(contest "ALLJA1 TEST" scoring
	(section "1エリア内 個人 電信限定 1.9MHz部門"         LB (verify (内? 信? 1.9MHz? LB?)))
	(section "1エリア内 個人 電信限定 3.5MHz部門"         LB (verify (内? 信? 3.5MHz? LB?)))
	(section "1エリア内 個人 電信電話 3.5MHz部門"         LB (verify (内?     3.5MHz? LB?)))
	(section "1エリア内 個人 電信限定 7MHz部門"           LB (verify (内? 信?   7MHz? LB?)))
	(section "1エリア内 個人 電信電話 7MHz部門"           LB (verify (内?       7MHz? LB?)))
	(section "1エリア内 個人 電信限定 1.9/3.5/7MHz部門"   LB (verify (内? 信?         LB?)))
	(section "1エリア内 個人 電信電話 1.9/3.5/7MHz部門"   LB (verify (内?             LB?)))

	(section "1エリア外 個人 電信限定 1.9MHz部門"         LB (verify (外? 信? 1.9MHz? LB?)))
	(section "1エリア外 個人 電信限定 3.5MHz部門"         LB (verify (外? 信? 3.5MHz? LB?)))
	(section "1エリア外 個人 電信電話 3.5MHz部門"         LB (verify (外?     3.5MHz? LB?)))
	(section "1エリア外 個人 電信限定 7MHz部門"           LB (verify (外? 信?   7MHz? LB?)))
	(section "1エリア外 個人 電信電話 7MHz部門"           LB (verify (外?       7MHz? LB?)))
	(section "1エリア外 個人 電信限定 1.9/3.5/7MHz部門"   LB (verify (外? 信?         LB?)))
	(section "1エリア外 個人 電信電話 1.9/3.5/7MHz部門"   LB (verify (外?             LB?)))

	(section "1エリア内 個人 電信限定 14MHz部門"          HB (verify (内? 信?  14MHz? HB?)))
	(section "1エリア内 個人 電信電話 14MHz部門"          HB (verify (内?      14MHz? HB?)))
	(section "1エリア内 個人 電信限定 21MHz部門"          HB (verify (内? 信?  21MHz? HB?)))
	(section "1エリア内 個人 電信電話 21MHz部門"          HB (verify (内?      21MHz? HB?)))
	(section "1エリア内 個人 電信限定 28MHz部門"          HB (verify (内? 信?  28MHz? HB?)))
	(section "1エリア内 個人 電信電話 28MHz部門"          HB (verify (内?      28MHz? HB?)))
	(section "1エリア内 個人 電信限定 50MHz部門"          HB (verify (内? 信?  50MHz? HB?)))
	(section "1エリア内 個人 電信電話 50MHz部門"          HB (verify (内?      50MHz? HB?)))
	(section "1エリア内 個人 電信限定 14/21/28/50MHz部門" HB (verify (内? 信?         HB?)))
	(section "1エリア内 個人 電信電話 14/21/28/50MHz部門" HB (verify (内?             HB?)))

	(section "1エリア外 個人 電信限定 14MHz部門"          HB (verify (外? 信?  14MHz? HB?)))
	(section "1エリア外 個人 電信電話 14MHz部門"          HB (verify (外?      14MHz? HB?)))
	(section "1エリア外 個人 電信限定 21MHz部門"          HB (verify (外? 信?  21MHz? HB?)))
	(section "1エリア外 個人 電信電話 21MHz部門"          HB (verify (外?      21MHz? HB?)))
	(section "1エリア外 個人 電信限定 28MHz部門"          HB (verify (外? 信?  28MHz? HB?)))
	(section "1エリア外 個人 電信電話 28MHz部門"          HB (verify (外?      28MHz? HB?)))
	(section "1エリア外 個人 電信限定 50MHz部門"          HB (verify (外? 信?  50MHz? HB?)))
	(section "1エリア外 個人 電信電話 50MHz部門"          HB (verify (外?      50MHz? HB?)))
	(section "1エリア外 個人 電信限定 14/21/28/50MHz部門" HB (verify (外? 信?         HB?)))
	(section "1エリア外 個人 電信電話 14/21/28/50MHz部門" HB (verify (外?             HB?)))

	(section "1エリア内 団体 電信限定 部門"               AB (verify (内? 信?         AB?)))
	(section "1エリア内 団体 電信電話 部門"               AB (verify (内?             AB?)))
	(section "1エリア外 団体 電信限定 部門"               AB (verify (外? 信?         AB?)))
	(section "1エリア外 団体 電信電話 部門"               AB (verify (外?             AB?)))

	(section "1エリア内 個人 デジタル 部門"               DB (verify (内?       7MHz? DB?)))
	(section "1エリア外 個人 デジタル 部門"               DB (verify (外?       7MHz? DB?)))
	(section "1エリア内 団体 デジタル 部門"               DB (verify (内?       7MHz? DB?)))
	(section "1エリア外 団体 デジタル 部門"               DB (verify (外?       7MHz? DB?))))
