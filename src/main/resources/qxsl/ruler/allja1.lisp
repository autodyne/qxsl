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
	(if
		(qxsl? it)
		(progn
			(setq freq (qxsl-band it))
			(cond (
				((<=  1810 freq 1913) "160m")
				((<=  3500 freq 3687)  "80m")
				((<=  7000 freq 7200)  "40m")
				((<= 14000 freq 14350) "20m")
				((<= 21000 freq 21450) "15m")
				((<= 28000 freq 29700) "10m")
				((<= 50000 freq 54000)  "6m"))))
		(adif-band it)))

; JCC/JCG
(defun jccg it
	(if
		(null? (rstq it))
		(str-tail (code it) (if (CW? it) 3 2))
		(code it)))

; conversion of JCC/JCG to city/prefecture name
(defun jarl-city it (city "jarl" (jccg it) 1))
(defun jarl-pref it (city "jarl" (jccg it) 0))

; time validation
(defun AM? it (member (time it) '(09 10 11)))
(defun PM? it (member (time it) '(16 17 18 19)))

; mode validation
(defun CW? it (member (mode it) '("cw" "CW")))

; JCC/JCG validation
(defun 都道府県? it (equal (jarl-city it) (jarl-pref it)))
(defun 市郡区? it (not (都道府県? it)))
(defun 支庁? it (match "1\\d{2}" (jccg it)))

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
	(cond(
		((北海道? it) (支庁? it))
		((関東甲? it) (市郡区? it))
		((他地方? it) (都道府県? it)))))

(defun 外? it (and (関東甲? it) (市郡区? it)))

; single lower-band sections
(defun 1.9MHz? it (and (PM? it) (equal (band it) "160m")))
(defun 3.5MHz? it (and (PM? it) (equal (band it)  "80m")))
(defun   7MHz? it (and (PM? it) (equal (band it)  "40m")))

; single UPPER-BAND sections
(defun  14MHz? it (and (AM? it) (equal (band it)  "20m")))
(defun  21MHz? it (and (AM? it) (equal (band it)  "15m")))
(defun  28MHz? it (and (AM? it) (equal (band it)  "10m")))
(defun  50MHz? it (and (AM? it) (equal (band it)   "6m")))

; multi-band sections
(defun 低周波? it (or (1.9MHz? it) (3.5MHz? it) ( 7MHz? it)))
(defun 高周波? it (or ( 14MHz? it) ( 21MHz? it) (28MHz? it) (50MHz? it)))
(defun 全周波? it (or (低周波? it) (高周波? it)))

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
	(section "1エリア内 個人 電信限定 1.9MHz部門"         (lambda it (verify it (list 内? CW? 1.9MHz?))))
	(section "1エリア内 個人 電信限定 3.5MHz部門"         (lambda it (verify it (list 内? CW? 3.5MHz?))))
	(section "1エリア内 個人 電信限定 7MHz部門"           (lambda it (verify it (list 内? CW?   7MHz?))))
	(section "1エリア内 個人 電信電話 3.5MHz部門"         (lambda it (verify it (list 内?     3.5MHz?))))
	(section "1エリア内 個人 電信電話 7MHz部門"           (lambda it (verify it (list 内?       7MHz?))))
	(section "1エリア内 個人 電信限定 1.9/3.5/7MHz部門"   (lambda it (verify it (list 内? CW? 低周波?))))
	(section "1エリア内 個人 電信電話 1.9/3.5/7MHz部門"   (lambda it (verify it (list 内?     低周波?))))

	(section "1エリア外 個人 電信限定 1.9MHz部門"         (lambda it (verify it (list 外? CW? 1.9MHz?))))
	(section "1エリア外 個人 電信限定 3.5MHz部門"         (lambda it (verify it (list 外? CW? 3.5MHz?))))
	(section "1エリア外 個人 電信限定 7MHz部門"           (lambda it (verify it (list 外? CW?   7MHz?))))
	(section "1エリア外 個人 電信電話 3.5MHz部門"         (lambda it (verify it (list 外?     3.5MHz?))))
	(section "1エリア外 個人 電信電話 7MHz部門"           (lambda it (verify it (list 外?       7MHz?))))
	(section "1エリア外 個人 電信限定 1.9/3.5/7MHz部門"   (lambda it (verify it (list 外? CW? 低周波?))))
	(section "1エリア外 個人 電信電話 1.9/3.5/7MHz部門"   (lambda it (verify it (list 外?     低周波?))))

	(section "1エリア内 個人 電信限定 14MHz部門"          (lambda it (verify it (list 内? CW?  14MHz?))))
	(section "1エリア内 個人 電信限定 21MHz部門"          (lambda it (verify it (list 内? CW?  21MHz?))))
	(section "1エリア内 個人 電信限定 28MHz部門"          (lambda it (verify it (list 内? CW?  28MHz?))))
	(section "1エリア内 個人 電信限定 50MHz部門"          (lambda it (verify it (list 内? CW?  50MHz?))))
	(section "1エリア内 個人 電信電話 14MHz部門"          (lambda it (verify it (list 内?      14MHz?))))
	(section "1エリア内 個人 電信電話 21MHz部門"          (lambda it (verify it (list 内?      21MHz?))))
	(section "1エリア内 個人 電信電話 28MHz部門"          (lambda it (verify it (list 内?      28MHz?))))
	(section "1エリア内 個人 電信電話 50MHz部門"          (lambda it (verify it (list 内?      50MHz?))))
	(section "1エリア内 個人 電信限定 14/21/28/50MHz部門" (lambda it (verify it (list 内? CW? 高周波?))))
	(section "1エリア内 個人 電信電話 14/21/28/50MHz部門" (lambda it (verify it (list 内?     高周波?))))

	(section "1エリア外 個人 電信限定 14MHz部門"          (lambda it (verify it (list 外? CW?  14MHz?))))
	(section "1エリア外 個人 電信限定 21MHz部門"          (lambda it (verify it (list 外? CW?  21MHz?))))
	(section "1エリア外 個人 電信限定 28MHz部門"          (lambda it (verify it (list 外? CW?  28MHz?))))
	(section "1エリア外 個人 電信限定 50MHz部門"          (lambda it (verify it (list 外? CW?  50MHz?))))
	(section "1エリア外 個人 電信電話 14MHz部門"          (lambda it (verify it (list 外?      14MHz?))))
	(section "1エリア外 個人 電信電話 21MHz部門"          (lambda it (verify it (list 外?      21MHz?))))
	(section "1エリア外 個人 電信電話 28MHz部門"          (lambda it (verify it (list 外?      28MHz?))))
	(section "1エリア外 個人 電信電話 50MHz部門"          (lambda it (verify it (list 外?      50MHz?))))
	(section "1エリア外 個人 電信限定 14/21/28/50MHz部門" (lambda it (verify it (list 外? CW? 高周波?))))
	(section "1エリア外 個人 電信電話 14/21/28/50MHz部門" (lambda it (verify it (list 外?     高周波?))))

	(section "1エリア内 個人 電信限定 総合部門"           (lambda it (verify it (list 内? CW? 全周波?))))
	(section "1エリア内 個人 電信電話 総合部門"           (lambda it (verify it (list 内?     全周波?))))
	(section "1エリア外 個人 電信限定 総合部門"           (lambda it (verify it (list 外? CW? 全周波?))))
	(section "1エリア外 個人 電信電話 総合部門"           (lambda it (verify it (list 外?     全周波?))))

	(section "1エリア内 社団 電信限定 オールバンド部門"   (lambda it (verify it (list 内? CW? 全周波?))))
	(section "1エリア内 社団 電信電話 オールバンド部門"   (lambda it (verify it (list 内?     全周波?))))
	(section "1エリア外 社団 電信限定 オールバンド部門"   (lambda it (verify it (list 外? CW? 全周波?))))
	(section "1エリア外 社団 電信電話 オールバンド部門"   (lambda it (verify it (list 外?     全周波?)))))
