;; ALLJA1 CONTEST DEFINITION by 無線部開発班

; utility macros
(set 'setq
	(syntax (name value)
		`(set ',name ,value)))

(setq defun
	(syntax (name pars body)
		`(setq ,name (lambda ,pars ,body))))

(setq cond
	(syntax (conds)
		(if
			(empty? conds)
			null
			`(if
				,(car (car conds))
				,(car (cdr (car conds)))
				(cond ,(cdr conds))))))

; utility functions
(defun null? value (equal null value))

(defun forall (it conds)
	(if
		(empty? conds)
		true
		(and
			((car conds) it)
			(forall it (cdr conds)))))

; XML namespaces
(setq qxsl "qxsl.org")
(setq adif "adif.org")

; field access for qxsl.org fields
(defun qxsl-time it (get-field it qxsl "time"))
(defun qxsl-call it (get-field it qxsl "call"))
(defun qxsl-band it (get-field it qxsl "band"))
(defun qxsl-mode it (get-field it qxsl "mode"))
(defun qxsl-rstq it (get-field (rcvd it) qxsl "rstq"))
(defun qxsl-code it (get-field (rcvd it) qxsl "code"))

; field access for adif.org fields
(defun adif-time it (get-field it adif "TIME_ON"))
(defun adif-call it (get-field it adif "CALL"))
(defun adif-band it (get-field it adif "BAND"))
(defun adif-mode it (get-field it adif "MODE"))
(defun adif-rstq it (get-field it adif "RST_RCVD"))
(defun adif-code it (get-field it adif "SRX"))

; conversion of time to hour (involving UTC-JST conversion for ADIF)
(defun qxsl-hour it (hour (qxsl-time it) "JST"))
(defun adif-hour it (mod (+ 9 (number (str-head (adif-time it) 2))) 24))

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
			((or (and (qxsl? it) (<=  1810 freq  1913)) (equal band "160m")) 1.9)
			((or (and (qxsl? it) (<=  3500 freq  3687)) (equal band  "80m")) 3.5)
			((or (and (qxsl? it) (<=  7000 freq  7200)) (equal band  "40m"))   7)
			((or (and (qxsl? it) (<= 14000 freq 14350)) (equal band  "20m"))  14)
			((or (and (qxsl? it) (<= 21000 freq 21450)) (equal band  "15m"))  21)
			((or (and (qxsl? it) (<= 28000 freq 29700)) (equal band  "10m"))  28)
			((or (and (qxsl? it) (<= 50000 freq 54000)) (equal band   "6m"))  50)))))

; single lower-band sections
(defun 1.9MHz? it (equal (band it) 1.9))
(defun 3.5MHz? it (equal (band it) 3.5))
(defun   7MHz? it (equal (band it)   7))

; single UPPER-BAND sections
(defun  14MHz? it (equal (band it)  14))
(defun  21MHz? it (equal (band it)  21))
(defun  28MHz? it (equal (band it)  28))
(defun  50MHz? it (equal (band it)  50))

; JCC/JCG
(defun jccg it
	(if
		(null? (rstq it))
		(str-tail (code it) (if (CW? it) 3 2))
		(code it)))

; conversion of JCC/JCG to city/prefecture name
(defun jarl-city it (city "jarl" (jccg it)))
(defun jarl-pref it (city "jarl" (jccg it) 0))

; time validation
(defun AM? it (and (<= 09 (time it) 11) (<=  14 (band it) 50)))
(defun PM? it (and (<= 16 (time it) 19) (<= 1.9 (band it)  7)))
(defun Dg? it (and (<= 13 (time it) 14) (<= 3.5 (band it) 28)))
(defun An? it (or (AM? it) (PM? it)))

; mode validation
(defun CW? it (member (mode it) '("CW")))
(defun FT? it (match "FT[48]" (mode it)))

; JCC/JCG validation
(defun 支庁? it (match "1\\d{2}" (jccg it)))
(defun 府県? it (equal (jarl-city it) (jarl-pref it)))
(defun 市郡? it (not (or (府県? it) (equal (jarl-city it) "東京都小笠原"))))

(defun 関東甲? it
	(member (jarl-pref it)
		(list
			"東京都"
			"神奈川県"
			"埼玉県"
			"千葉県"
			"群馬県"
			"茨城県"
			"栃木県"
			"山梨県")))
(defun 北海道? it (equal (jarl-pref it) "北海道"))
(defun 他地方? it (not (or (関東甲? it) (北海道? it))))

;; 1エリア内/1エリア外部門
(defun 内? it
	(if
		(Dg? it)
		(市郡? it)
		(cond(
			((北海道? it) (支庁? it))
			((関東甲? it) (市郡? it))
			((他地方? it) (府県? it))))))

(defun 外? it (and (関東甲? it) (市郡? it)))

; key for scoring
(defun CALL-KEY it (list (call it) (band it) (mode it)))
(defun MULT-KEY it (list (band it) (jccg it)))

; validation
(defun verify (it conds)
	(if
		(forall it conds)
		(success it 1 (CALL-KEY it) (MULT-KEY it))
		(failure it "無効な交信")))

(contest "ALLJA1 TEST"
	(section "1エリア内 個人 電信限定 1.9MHz部門"         (lambda it (verify it (list 内? CW? 1.9MHz? PM?))))
	(section "1エリア内 個人 電信限定 3.5MHz部門"         (lambda it (verify it (list 内? CW? 3.5MHz? PM?))))
	(section "1エリア内 個人 電信電話 3.5MHz部門"         (lambda it (verify it (list 内?     3.5MHz? PM?))))
	(section "1エリア内 個人 電信限定 7MHz部門"           (lambda it (verify it (list 内? CW?   7MHz? PM?))))
	(section "1エリア内 個人 電信電話 7MHz部門"           (lambda it (verify it (list 内?       7MHz? PM?))))
	(section "1エリア内 個人 電信限定 1.9/3.5/7MHz部門"   (lambda it (verify it (list 内? CW?         PM?))))
	(section "1エリア内 個人 電信電話 1.9/3.5/7MHz部門"   (lambda it (verify it (list 内?             PM?))))

	(section "1エリア外 個人 電信限定 1.9MHz部門"         (lambda it (verify it (list 外? CW? 1.9MHz? PM?))))
	(section "1エリア外 個人 電信限定 3.5MHz部門"         (lambda it (verify it (list 外? CW? 3.5MHz? PM?))))
	(section "1エリア外 個人 電信電話 3.5MHz部門"         (lambda it (verify it (list 外?     3.5MHz? PM?))))
	(section "1エリア外 個人 電信限定 7MHz部門"           (lambda it (verify it (list 外? CW?   7MHz? PM?))))
	(section "1エリア外 個人 電信電話 7MHz部門"           (lambda it (verify it (list 外?       7MHz? PM?))))
	(section "1エリア外 個人 電信限定 1.9/3.5/7MHz部門"   (lambda it (verify it (list 外? CW?         PM?))))
	(section "1エリア外 個人 電信電話 1.9/3.5/7MHz部門"   (lambda it (verify it (list 外?             PM?))))

	(section "1エリア内 個人 電信限定 14MHz部門"          (lambda it (verify it (list 内? CW?  14MHz? AM?))))
	(section "1エリア内 個人 電信電話 14MHz部門"          (lambda it (verify it (list 内?      14MHz? AM?))))
	(section "1エリア内 個人 電信限定 21MHz部門"          (lambda it (verify it (list 内? CW?  21MHz? AM?))))
	(section "1エリア内 個人 電信電話 21MHz部門"          (lambda it (verify it (list 内?      21MHz? AM?))))
	(section "1エリア内 個人 電信限定 28MHz部門"          (lambda it (verify it (list 内? CW?  28MHz? AM?))))
	(section "1エリア内 個人 電信電話 28MHz部門"          (lambda it (verify it (list 内?      28MHz? AM?))))
	(section "1エリア内 個人 電信限定 50MHz部門"          (lambda it (verify it (list 内? CW?  50MHz? AM?))))
	(section "1エリア内 個人 電信電話 50MHz部門"          (lambda it (verify it (list 内?      50MHz? AM?))))
	(section "1エリア内 個人 電信限定 14/21/28/50MHz部門" (lambda it (verify it (list 内? CW?         AM?))))
	(section "1エリア内 個人 電信電話 14/21/28/50MHz部門" (lambda it (verify it (list 内?             AM?))))

	(section "1エリア外 個人 電信限定 14MHz部門"          (lambda it (verify it (list 外? CW?  14MHz? AM?))))
	(section "1エリア外 個人 電信電話 14MHz部門"          (lambda it (verify it (list 外?      14MHz? AM?))))
	(section "1エリア外 個人 電信限定 21MHz部門"          (lambda it (verify it (list 外? CW?  21MHz? AM?))))
	(section "1エリア外 個人 電信電話 21MHz部門"          (lambda it (verify it (list 外?      21MHz? AM?))))
	(section "1エリア外 個人 電信限定 28MHz部門"          (lambda it (verify it (list 外? CW?  28MHz? AM?))))
	(section "1エリア外 個人 電信電話 28MHz部門"          (lambda it (verify it (list 外?      28MHz? AM?))))
	(section "1エリア外 個人 電信限定 50MHz部門"          (lambda it (verify it (list 外? CW?  50MHz? AM?))))
	(section "1エリア外 個人 電信電話 50MHz部門"          (lambda it (verify it (list 外?      50MHz? AM?))))
	(section "1エリア外 個人 電信限定 14/21/28/50MHz部門" (lambda it (verify it (list 外? CW?         AM?))))
	(section "1エリア外 個人 電信電話 14/21/28/50MHz部門" (lambda it (verify it (list 外?             AM?))))

	(section "1エリア内 団体 電信限定 オールバンド部門"   (lambda it (verify it (list 内? CW?         An?))))
	(section "1エリア内 団体 電信電話 オールバンド部門"   (lambda it (verify it (list 内?             An?))))
	(section "1エリア外 団体 電信限定 オールバンド部門"   (lambda it (verify it (list 外? CW?         An?))))
	(section "1エリア外 団体 電信電話 オールバンド部門"   (lambda it (verify it (list 外?             An?))))

	(section "1エリア内 個人 デジタル オールバンド部門"   (lambda it (verify it (list 内? FT?         Dg?))))
	(section "1エリア外 個人 デジタル オールバンド部門"   (lambda it (verify it (list 外? FT?         Dg?))))
	(section "1エリア外 団体 デジタル オールバンド部門"   (lambda it (verify it (list 外? FT?         Dg?))))
	(section "1エリア外 団体 デジタル オールバンド部門"   (lambda it (verify it (list 外? FT?         Dg?)))))
