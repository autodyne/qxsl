;; ALLJA1 CONTEST DEFINED by 無線部開発班

(load "format.lisp")

; UTC-JST conversion for ADIF
(defun HOUR it (hour (qxsl-time it) "JST"))

; mode validation
(defun 信? it (match "(?i)CW" (qxsl-mode it)))
(defun 話? it (match "(?i)PH|FM|SSB" (qxsl-mode it)))
(defun 離? it (match "(?i)DG|FT[48]" (qxsl-mode it)))
(defun 連? it (or (信? it) (話? it)))

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
(defun HB? it (forall it (朝? 連? 高?)))
(defun LB? it (forall it (夜? 連? 低?)))
(defun DS? it (forall it (昼? 離? 7MHz?)))
(defun AB? it (or (HB? it) (LB? it)))
(defun JS? it (or (HB? it) (LB? it) (DS? it)))

; JCC/JCG
(defun peel-JCCG it
	(car
		(cdr
			(tokenize
				(if (話? it) "^.." "^...")
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
(defun 個? it #t)
(defun 団? it (not (null? (qxsl-name it))))

; keys for scoring
(defun MODES-key it
	(cond (
		((信? it) 1)
		((話? it) 2)
		((離? it) 3))))
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

(setq 個 (list CALLS-key MULTS-key DUMMY-key))
(setq 団 (list CALLS-key MULTS-key PARTY-key))

; scoring
(defmacro scoring (score calls mults names)
	`(progn
		(setq mults (length (quote ,mults)))
		(setq names (length (quote ,names)))
		(ceiling (/ (* ,score mults) names))))

; validation routine
(defmacro verify (conds keys)
	`(lambda it
		(progn
			(toQXSL it)
			(if
				(forall it ,conds)
				(success it 1 ,@(dolist (k (eval keys)) (list k 'it)))
				(failure it "無効な交信")))))

; section codes
(setq HB "アナログ ハイバンド部門")
(setq LB "アナログ ローバンド部門")
(setq AB "アナログ 全周波数帯部門")
(setq DS "デジタル 全周波数帯部門")
(setq JS "総合部門")

(contest "ALLJA1 TEST" scoring
	(section "1エリア内 個人 電信限定 1.9MHz部門"         LB (verify (個? 内? 信? 1.9MHz? LB?) 個))
	(section "1エリア内 個人 電信限定 3.5MHz部門"         LB (verify (個? 内? 信? 3.5MHz? LB?) 個))
	(section "1エリア内 個人 電信電話 3.5MHz部門"         LB (verify (個? 内?     3.5MHz? LB?) 個))
	(section "1エリア内 個人 電信限定 7MHz部門"           LB (verify (個? 内? 信?   7MHz? LB?) 個))
	(section "1エリア内 個人 電信電話 7MHz部門"           LB (verify (個? 内?       7MHz? LB?) 個))
	(section "1エリア内 個人 電信限定 1.9/3.5/7MHz部門"   LB (verify (個? 内? 信?         LB?) 個))
	(section "1エリア内 個人 電信電話 1.9/3.5/7MHz部門"   LB (verify (個? 内?             LB?) 個))

	(section "1エリア外 個人 電信限定 1.9MHz部門"         LB (verify (個? 外? 信? 1.9MHz? LB?) 個))
	(section "1エリア外 個人 電信限定 3.5MHz部門"         LB (verify (個? 外? 信? 3.5MHz? LB?) 個))
	(section "1エリア外 個人 電信電話 3.5MHz部門"         LB (verify (個? 外?     3.5MHz? LB?) 個))
	(section "1エリア外 個人 電信限定 7MHz部門"           LB (verify (個? 外? 信?   7MHz? LB?) 個))
	(section "1エリア外 個人 電信電話 7MHz部門"           LB (verify (個? 外?       7MHz? LB?) 個))
	(section "1エリア外 個人 電信限定 1.9/3.5/7MHz部門"   LB (verify (個? 外? 信?         LB?) 個))
	(section "1エリア外 個人 電信電話 1.9/3.5/7MHz部門"   LB (verify (個? 外?             LB?) 個))

	(section "1エリア内 個人 電信限定 14MHz部門"          HB (verify (個? 内? 信?  14MHz? HB?) 個))
	(section "1エリア内 個人 電信電話 14MHz部門"          HB (verify (個? 内?      14MHz? HB?) 個))
	(section "1エリア内 個人 電信限定 21MHz部門"          HB (verify (個? 内? 信?  21MHz? HB?) 個))
	(section "1エリア内 個人 電信電話 21MHz部門"          HB (verify (個? 内?      21MHz? HB?) 個))
	(section "1エリア内 個人 電信限定 28MHz部門"          HB (verify (個? 内? 信?  28MHz? HB?) 個))
	(section "1エリア内 個人 電信電話 28MHz部門"          HB (verify (個? 内?      28MHz? HB?) 個))
	(section "1エリア内 個人 電信限定 50MHz部門"          HB (verify (個? 内? 信?  50MHz? HB?) 個))
	(section "1エリア内 個人 電信電話 50MHz部門"          HB (verify (個? 内?      50MHz? HB?) 個))
	(section "1エリア内 個人 電信限定 14/21/28/50MHz部門" HB (verify (個? 内? 信?         HB?) 個))
	(section "1エリア内 個人 電信電話 14/21/28/50MHz部門" HB (verify (個? 内?             HB?) 個))

	(section "1エリア外 個人 電信限定 14MHz部門"          HB (verify (個? 外? 信?  14MHz? HB?) 個))
	(section "1エリア外 個人 電信電話 14MHz部門"          HB (verify (個? 外?      14MHz? HB?) 個))
	(section "1エリア外 個人 電信限定 21MHz部門"          HB (verify (個? 外? 信?  21MHz? HB?) 個))
	(section "1エリア外 個人 電信電話 21MHz部門"          HB (verify (個? 外?      21MHz? HB?) 個))
	(section "1エリア外 個人 電信限定 28MHz部門"          HB (verify (個? 外? 信?  28MHz? HB?) 個))
	(section "1エリア外 個人 電信電話 28MHz部門"          HB (verify (個? 外?      28MHz? HB?) 個))
	(section "1エリア外 個人 電信限定 50MHz部門"          HB (verify (個? 外? 信?  50MHz? HB?) 個))
	(section "1エリア外 個人 電信電話 50MHz部門"          HB (verify (個? 外?      50MHz? HB?) 個))
	(section "1エリア外 個人 電信限定 14/21/28/50MHz部門" HB (verify (個? 外? 信?         HB?) 個))
	(section "1エリア外 個人 電信電話 14/21/28/50MHz部門" HB (verify (個? 外?             HB?) 個))

	(section "1エリア内 団体 電信限定 部門"               AB (verify (団? 内? 信?         AB?) 団))
	(section "1エリア内 団体 電信電話 部門"               AB (verify (団? 内?             AB?) 団))
	(section "1エリア外 団体 電信限定 部門"               AB (verify (団? 外? 信?         AB?) 団))
	(section "1エリア外 団体 電信電話 部門"               AB (verify (団? 外?             AB?) 団))

	(section "1エリア内 個人 デジタル 部門"               DS (verify (個? 内?             DS?) 個))
	(section "1エリア外 個人 デジタル 部門"               DS (verify (個? 外?             DS?) 個))
	(section "1エリア内 団体 デジタル 部門"               DS (verify (団? 内?             DS?) 団))
	(section "1エリア外 団体 デジタル 部門"               DS (verify (団? 外?             DS?) 団))

	(section "1エリア内 個人 総合 部門"                   JS (verify (個? 内?             JS?) 個))
	(section "1エリア外 個人 総合 部門"                   JS (verify (個? 外?             JS?) 個))
	(section "1エリア内 団体 総合 部門"                   JS (verify (団? 内?             JS?) 団))
	(section "1エリア外 団体 総合 部門"                   JS (verify (団? 外?             JS?) 団)))
