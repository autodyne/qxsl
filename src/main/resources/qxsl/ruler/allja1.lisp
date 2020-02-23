;; ALLJA1 CONTEST DEFINITION by 無線部開発班

; utility macros
(set 'setq
	(syntax (name value)
		`(set ',name ,value)))

(setq defun
	(syntax (name pars body)
		`(setq ,name (lambda ,pars ,body))))

(setq defmacro
	(syntax (name pars body)
		`(setq ,name (syntax ,pars ,body))))

(defmacro cond conds
	(if
		(empty? conds)
		null
		`(if
			,(car (car conds))
			,(car (cdr (car conds)))
			(cond ,(cdr conds)))))

(defmacro forall (it conds)
	(if
		(empty? conds)
		true
		`(and
			(,(car conds) ,it)
			(forall ,it ,(cdr conds)))))

; XML namespaces
(setq qxsl "qxsl.org")
(setq adif "adif.org")

; field access for qxsl.org
(defun qxsl-time it (get-field it qxsl "time"))
(defun qxsl-call it (get-field it qxsl "call"))
(defun qxsl-band it (get-field it qxsl "band"))
(defun qxsl-mode it (get-field it qxsl "mode"))
(defun qxsl-rstq it (get-field (rcvd it) qxsl "rstq"))
(defun qxsl-code it (get-field (rcvd it) qxsl "code"))

; field access for adif.org
(defun adif-time it (get-field it adif "TIME_ON"))
(defun adif-call it (get-field it adif "CALL"))
(defun adif-band it (get-field it adif "BAND"))
(defun adif-mode it (get-field it adif "MODE"))
(defun adif-rstq it (get-field it adif "RST_RCVD"))
(defun adif-code it (get-field it adif "SRX"))

; conversion of time to hour (involving UTC-JST conversion for ADIF)
(defun qxsl-hour it (hour (qxsl-time it) "JST"))
(defun adif-hour it (mod (+ 9 (number (substring (adif-time it) 0 2))) 24))

; ADIF/QXSL checker
(defun qxsl? it (null? (adif-time it)))

; field access for both ADIF/QXSL
(defun time it ((if (qxsl? it) qxsl-hour adif-hour) it))
(defun call it ((if (qxsl? it) qxsl-call adif-call) it))
(defun mode it ((if (qxsl? it) qxsl-mode adif-mode) it))
(defun rstq it ((if (qxsl? it) qxsl-rstq adif-rstq) it))
(defun code it ((if (qxsl? it) qxsl-code adif-code) it))

; conversion of qxsl's band (kHz) to ADIF band
(defun band it
	(progn
		(setq freq (qxsl-band it))
		(setq band (adif-band it))
		(cond (
			((if (qxsl? it) (<=  1810 freq  1913) (equal band "160m")) 1.9)
			((if (qxsl? it) (<=  3500 freq  3687) (equal band  "80m")) 3.5)
			((if (qxsl? it) (<=  7000 freq  7200) (equal band  "40m"))   7)
			((if (qxsl? it) (<= 14000 freq 14350) (equal band  "20m"))  14)
			((if (qxsl? it) (<= 21000 freq 21450) (equal band  "15m"))  21)
			((if (qxsl? it) (<= 28000 freq 29700) (equal band  "10m"))  28)
			((if (qxsl? it) (<= 50000 freq 54000) (equal band   "6m"))  50)))))

; JCC/JCG
(defun jccg it
	(if
		(null? (rstq it))
		(car (cdr (tokenize (if (CW? it) "^..." "^..") (code it))))
		(code it)))

; conversion of JCC/JCG to city/prefecture name
(defun 県 it (city "jarl" (jccg it) 0))
(defun 市 it (city "jarl" (jccg it)))
(defun 総 it (city "area" (県 it) 2))

; mode validation
(defun CW? it (match "(?i)CW" (mode it)))
(defun Dg? it (match "(?i)(DG|FT4|FT8)" (mode it)))
(defun PH? it (match "(?i)(PH|AM|FM|SSB|DSB|LSB|USB)" (mode it)))
(defun An? it (or (CW? it) (PH? it)))

; time validation
(defun HB? it (and (<= 09 (time it) 11) (An? it) (<=  14 (band it) 50)))
(defun LB? it (and (<= 16 (time it) 19) (An? it) (<= 1.9 (band it)  7)))
(defun DB? it (and (<= 13 (time it) 14) (Dg? it)))
(defun AB? it (or (HB? it) (LB? it)))

; single lower-band validation
(defun 1.9MHz? it (equal (band it) 1.9))
(defun 3.5MHz? it (equal (band it) 3.5))
(defun   7MHz? it (equal (band it)   7))

; single UPPER-BAND validation
(defun  14MHz? it (equal (band it)  14))
(defun  21MHz? it (equal (band it)  21))
(defun  28MHz? it (equal (band it)  28))
(defun  50MHz? it (equal (band it)  50))

; JCC/JCG validation
(defun 現存? it (not (null? (市 it))))
(defun 支庁? it (match "\\d{3}"  (jccg it)))
(defun 府県? it (match "\\d{2}"  (jccg it)))
(defun 市郡? it (match "\\d{4,}" (jccg it)))

(defun 関? it (equal (総 it) "関東"))
(defun 道? it (equal (県 it) "北海道"))
(defun 他? it (not (or (関? it) (道? it))))

;; 1エリア内/1エリア外部門
(defun 内? it
	(and
		(現存? it)
		(cond (
			((Dg? it) (市郡? it))
			((道? it) (支庁? it))
			((関? it) (市郡? it))
			((他? it) (府県? it))))))

(defun 外? it (and (現存? it) (関? it) (市郡? it)))

; key for scoring
(defun MODE-KEY it (cond (((CW? it) 1) ((PH? it) 2) ((Dg? it) 3))))
(defun CALL-KEY it (list (call it) (band it) (MODE-KEY it)))
(defun MULT-KEY it (list (band it) (jccg it)))

; validation
(defmacro verify conds
	`(lambda it
		(if
			(forall it ,conds)
			(success it 1 (CALL-KEY it) (MULT-KEY it))
			(failure it "無効な交信"))))

; section codes
(setq HB "HB")
(setq LB "LB")
(setq DB "DB")
(setq AB "AB")

(contest "ALLJA1 TEST"
	(section "1エリア内 個人 電信限定 1.9MHz部門"         LB (verify (内? CW? 1.9MHz? LB?)))
	(section "1エリア内 個人 電信限定 3.5MHz部門"         LB (verify (内? CW? 3.5MHz? LB?)))
	(section "1エリア内 個人 電信電話 3.5MHz部門"         LB (verify (内?     3.5MHz? LB?)))
	(section "1エリア内 個人 電信限定 7MHz部門"           LB (verify (内? CW?   7MHz? LB?)))
	(section "1エリア内 個人 電信電話 7MHz部門"           LB (verify (内?       7MHz? LB?)))
	(section "1エリア内 個人 電信限定 1.9/3.5/7MHz部門"   LB (verify (内? CW?         LB?)))
	(section "1エリア内 個人 電信電話 1.9/3.5/7MHz部門"   LB (verify (内?             LB?)))

	(section "1エリア外 個人 電信限定 1.9MHz部門"         LB (verify (外? CW? 1.9MHz? LB?)))
	(section "1エリア外 個人 電信限定 3.5MHz部門"         LB (verify (外? CW? 3.5MHz? LB?)))
	(section "1エリア外 個人 電信電話 3.5MHz部門"         LB (verify (外?     3.5MHz? LB?)))
	(section "1エリア外 個人 電信限定 7MHz部門"           LB (verify (外? CW?   7MHz? LB?)))
	(section "1エリア外 個人 電信電話 7MHz部門"           LB (verify (外?       7MHz? LB?)))
	(section "1エリア外 個人 電信限定 1.9/3.5/7MHz部門"   LB (verify (外? CW?         LB?)))
	(section "1エリア外 個人 電信電話 1.9/3.5/7MHz部門"   LB (verify (外?             LB?)))

	(section "1エリア内 個人 電信限定 14MHz部門"          HB (verify (内? CW?  14MHz? HB?)))
	(section "1エリア内 個人 電信電話 14MHz部門"          HB (verify (内?      14MHz? HB?)))
	(section "1エリア内 個人 電信限定 21MHz部門"          HB (verify (内? CW?  21MHz? HB?)))
	(section "1エリア内 個人 電信電話 21MHz部門"          HB (verify (内?      21MHz? HB?)))
	(section "1エリア内 個人 電信限定 28MHz部門"          HB (verify (内? CW?  28MHz? HB?)))
	(section "1エリア内 個人 電信電話 28MHz部門"          HB (verify (内?      28MHz? HB?)))
	(section "1エリア内 個人 電信限定 50MHz部門"          HB (verify (内? CW?  50MHz? HB?)))
	(section "1エリア内 個人 電信電話 50MHz部門"          HB (verify (内?      50MHz? HB?)))
	(section "1エリア内 個人 電信限定 14/21/28/50MHz部門" HB (verify (内? CW?         HB?)))
	(section "1エリア内 個人 電信電話 14/21/28/50MHz部門" HB (verify (内?             HB?)))

	(section "1エリア外 個人 電信限定 14MHz部門"          HB (verify (外? CW?  14MHz? HB?)))
	(section "1エリア外 個人 電信電話 14MHz部門"          HB (verify (外?      14MHz? HB?)))
	(section "1エリア外 個人 電信限定 21MHz部門"          HB (verify (外? CW?  21MHz? HB?)))
	(section "1エリア外 個人 電信電話 21MHz部門"          HB (verify (外?      21MHz? HB?)))
	(section "1エリア外 個人 電信限定 28MHz部門"          HB (verify (外? CW?  28MHz? HB?)))
	(section "1エリア外 個人 電信電話 28MHz部門"          HB (verify (外?      28MHz? HB?)))
	(section "1エリア外 個人 電信限定 50MHz部門"          HB (verify (外? CW?  50MHz? HB?)))
	(section "1エリア外 個人 電信電話 50MHz部門"          HB (verify (外?      50MHz? HB?)))
	(section "1エリア外 個人 電信限定 14/21/28/50MHz部門" HB (verify (外? CW?         HB?)))
	(section "1エリア外 個人 電信電話 14/21/28/50MHz部門" HB (verify (外?             HB?)))

	(section "1エリア内 団体 電信限定 部門"               AB (verify (内? CW?         AB?)))
	(section "1エリア内 団体 電信電話 部門"               AB (verify (内?             AB?)))
	(section "1エリア外 団体 電信限定 部門"               AB (verify (外? CW?         AB?)))
	(section "1エリア外 団体 電信電話 部門"               AB (verify (外?             AB?)))

	(section "1エリア内 個人 デジタル 部門"               DB (verify (内?       7MHz? DB?)))
	(section "1エリア外 個人 デジタル 部門"               DB (verify (外?       7MHz? DB?)))
	(section "1エリア内 団体 デジタル 部門"               DB (verify (内?       7MHz? DB?)))
	(section "1エリア外 団体 デジタル 部門"               DB (verify (外?       7MHz? DB?))))
