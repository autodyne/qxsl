;; FORMAT CONVERTER DEFINED by 無線部開発班

(load "qxsl/ruler/common.lisp")

; XML namespaces
(setq qxsl "qxsl.org")
(setq adif "adif.org")

;; GENERAL FIELD ACCESS ROUTINES

; field keys for qxsl
(setq key-qxsl-time ((access 'TIME Qxsl)))
(setq key-qxsl-call ((access 'CALL Qxsl)))
(setq key-qxsl-band ((access 'BAND Qxsl)))
(setq key-qxsl-mode ((access 'MODE Qxsl)))
(setq key-qxsl-name ((access 'NAME Qxsl)))
(setq key-qxsl-rstq ((access 'RSTQ Qxsl)))
(setq key-qxsl-code ((access 'CODE Qxsl)))

; field keys for adif
(setq key-adif-DATE (name adif "QSO_DATE"))
(setq key-adif-TIME (name adif "TIME_ON"))
(setq key-adif-call (name adif "CALL"))
(setq key-adif-band (name adif "BAND"))
(setq key-adif-mode (name adif "MODE"))
(setq key-adif-name (name adif "OPERATOR"))
(setq key-adif-rstq (name adif "RST_RCVD"))
(setq key-adif-RSTQ (name adif "RST_SENT"))
(setq key-adif-code (name adif "SRX"))
(setq key-adif-CODE (name adif "STX"))

; field access for qxsl
(defun qxsl-time it (getf it key-qxsl-time))
(defun qxsl-call it (getf it key-qxsl-call))
(defun qxsl-BAND it (getf it key-qxsl-band))
(defun qxsl-mode it (getf it key-qxsl-mode))
(defun qxsl-name it (getf it key-qxsl-name))
(defun qxsl-rstq it (getf (rcvd it) key-qxsl-rstq))
(defun qxsl-RSTQ it (getf (sent it) key-qxsl-rstq))
(defun qxsl-code it (getf (rcvd it) key-qxsl-code))
(defun qxsl-CODE it (getf (sent it) key-qxsl-code))

; field access for adif
(defun adif-DATE it (getf it key-adif-DATE))
(defun adif-TIME it (getf it key-adif-TIME))
(defun adif-call it (getf it key-adif-call))
(defun adif-BAND it (getf it key-adif-band))
(defun adif-mode it (getf it key-adif-mode))
(defun adif-name it (getf it key-adif-name))
(defun adif-rstq it (getf it key-adif-rstq))
(defun adif-RSTQ it (getf it key-adif-RSTQ))
(defun adif-code it (getf it key-adif-code))
(defun adif-CODE it (getf it key-adif-CODE))

; field set for qxsl
(defun set-qxsl-time (it val) (setf it key-qxsl-time val))
(defun set-qxsl-call (it val) (setf it key-qxsl-call val))
(defun set-qxsl-band (it val) (setf it key-qxsl-band val))
(defun set-qxsl-mode (it val) (setf it key-qxsl-mode val))
(defun set-qxsl-name (it val) (setf it key-qxsl-name val))
(defun set-qxsl-rstq (it val) (setf (rcvd it) key-qxsl-rstq val))
(defun set-qxsl-RSTQ (it val) (setf (sent it) key-qxsl-rstq val))
(defun set-qxsl-code (it val) (setf (rcvd it) key-qxsl-code val))
(defun set-qxsl-CODE (it val) (setf (sent it) key-qxsl-code val))

; field set for adif
(defun set-adif-DATE (it val) (setf it key-adif-DATE val))
(defun set-adif-TIME (it val) (setf it key-adif-TIME val))
(defun set-adif-call (it val) (setf it key-adif-call val))
(defun set-adif-BAND (it val) (setf it key-adif-band val))
(defun set-adif-mode (it val) (setf it key-adif-mode val))
(defun set-adif-name (it val) (setf it key-adif-name val))
(defun set-adif-rstq (it val) (setf it key-adif-rstq val))
(defun set-adif-RSTQ (it val) (setf it key-adif-RSTQ val))
(defun set-adif-code (it val) (setf it key-adif-code val))
(defun set-adif-CODE (it val) (setf it key-adif-CODE val))

;; TIME FIELD ACCESS ROUTINES

; time access for adif
(defun adif-dISO it (replace (adif-DATE it) "(?<=^.{4}|^.{6})" "-"))
(defun adif-tISO it (replace (adif-TIME it) "(?<=^.{2}|^.{4})" ":"))
(defun adif-time it (format "%sT%sZ" (adif-dISO it) (adif-tISO it)))

; time set for adif
(defun set-adif-time (it val)
	(let stmp (iso-8601 (at-zone val (zone "UTC")))
		(set-adif-DATE it (replace (substring stmp  0 10) "-" ""))
		(set-adif-TIME it (replace (substring stmp 11 19) ":" ""))))

;; BAND FIELD ACCESS ROUTINES

; band enumeration for qxsl
(setq 1.9MHz     1900)
(setq 3.5MHz     3500)
(setq   7MHz     7000)
(setq  10MHz    10000)
(setq  14MHz    14000)
(setq  18MHz    18000)
(setq  21MHz    21000)
(setq  28MHz    28000)
(setq  50MHz    50000)
(setq 144MHz   144000)
(setq 430MHz   430000)
(setq 1.2GHz  1200000)
(setq 2.4GHz  2400000)
(setq 5.6GHz  5600000)
(setq  10GHz 10000000)

; band enumeration for adif
(setq 1.9MHz-adif "160m")
(setq 3.5MHz-adif  "80m")
(setq   7MHz-adif  "40m")
(setq  10MHz-adif  "30m")
(setq  14MHz-adif  "20m")
(setq  18MHz-adif  "17m")
(setq  21MHz-adif  "15m")
(setq  28MHz-adif  "10m")
(setq  50MHz-adif   "6m")
(setq 144MHz-adif   "2m")
(setq 430MHz-adif "70cm")
(setq 1.2GHz-adif "23cm")
(setq 2.4GHz-adif "13cm")
(setq 5.6GHz-adif  "6cm")
(setq  10GHz-adif  "3cm")

; band enumeration for CQWW
(setq 1.9MHz-cqww     1800)
(setq 3.5MHz-cqww     3500)
(setq   7MHz-cqww     7000)
(setq  10MHz-cqww    10000)
(setq  14MHz-cqww    14000)
(setq  18MHz-cqww    18000)
(setq  21MHz-cqww    21000)
(setq  28MHz-cqww    28000)
(setq  50MHz-cqww    50000)
(setq 144MHz-cqww   144000)
(setq 430MHz-cqww   432000)
(setq 1.2GHz-cqww  1200000)
(setq 2.4GHz-cqww  2300000)
(setq 5.6GHz-cqww  5700000)
(setq  10GHz-cqww 10000000)

; band access for qxsl
(defun qxsl-band it
	(let freq (qxsl-BAND it)
		(cond
			((<=     1800 freq     1913) 1.9MHz)
			((<=     3500 freq     3687) 3.5MHz)
			((<=     7000 freq     7200)   7MHz)
			((<=    10000 freq    10150)  10MHz)
			((<=    14000 freq    14350)  14MHz)
			((<=    18000 freq    18168)  18MHz)
			((<=    21000 freq    21450)  21MHz)
			((<=    28000 freq    29700)  28MHz)
			((<=    50000 freq    54000)  50MHz)
			((<=   144000 freq   146000) 144MHz)
			((<=   430000 freq   440000) 430MHz)
			((<=  1200000 freq  1300000) 1.2GHz)
			((<=  2400000 freq  2450000) 2.4GHz)
			((<=  5600000 freq  5850000) 5.6GHz)
			((<= 10000000 freq 10250000)  10GHz))))

; band access for adif
(defun adif-band it
	(let band (adif-BAND it)
		(string (cond
			((equal band 1.9MHz-adif) 1.9MHz)
			((equal band 3.5MHz-adif) 3.5MHz)
			((equal band   7MHz-adif)   7MHz)
			((equal band  10MHz-adif)  10MHz)
			((equal band  14MHz-adif)  14MHz)
			((equal band  18MHz-adif)  18MHz)
			((equal band  21MHz-adif)  21MHz)
			((equal band  28MHz-adif)  28MHz)
			((equal band  50MHz-adif)  50MHz)
			((equal band 144MHz-adif) 144MHz)
			((equal band 430MHz-adif) 430MHz)
			((equal band 1.2GHz-adif) 1.2GHz)
			((equal band 2.4GHz-adif) 2.4GHz)
			((equal band 5.6GHz-adif) 5.6GHz)
			((equal band  10GHz-adif)  10GHz)))))

; band set for adif
(defun set-adif-band (it val)
	(set-adif-BAND it (cond
		((equal val 1.9MHz) 1.9MHz-adif)
		((equal val 3.5MHz) 3.5MHz-adif)
		((equal val   7MHz)   7MHz-adif)
		((equal val  10MHz)  10MHz-adif)
		((equal val  14MHz)  14MHz-adif)
		((equal val  18MHz)  18MHz-adif)
		((equal val  21MHz)  21MHz-adif)
		((equal val  28MHz)  28MHz-adif)
		((equal val  50MHz)  50MHz-adif)
		((equal val 144MHz) 144MHz-adif)
		((equal val 430MHz) 430MHz-adif)
		((equal val 1.2GHz) 1.2GHz-adif)
		((equal val 2.4GHz) 2.4GHz-adif)
		((equal val 5.6GHz) 5.6GHz-adif)
		((equal val  10GHz)  10GHz-adif))))

; band set for CQWW
(defun set-cqww-band (it val)
	(set-qxsl-band it (cond
		((equal val 1.9MHz) 1.9MHz-cqww)
		((equal val 3.5MHz) 3.5MHz-cqww)
		((equal val   7MHz)   7MHz-cqww)
		((equal val  10MHz)  10MHz-cqww)
		((equal val  14MHz)  14MHz-cqww)
		((equal val  18MHz)  18MHz-cqww)
		((equal val  21MHz)  21MHz-cqww)
		((equal val  28MHz)  28MHz-cqww)
		((equal val  50MHz)  50MHz-cqww)
		((equal val 144MHz) 144MHz-cqww)
		((equal val 430MHz) 430MHz-cqww)
		((equal val 1.2GHz) 1.2GHz-cqww)
		((equal val 2.4GHz) 2.4GHz-cqww)
		((equal val 5.6GHz) 5.6GHz-cqww)
		((equal val  10GHz)  10GHz-cqww))))

; band validation
(defun 1.9MHz? it (equal (qxsl-band it) 1.9MHz))
(defun 3.5MHz? it (equal (qxsl-band it) 3.5MHz))
(defun   7MHz? it (equal (qxsl-band it)   7MHz))
(defun  10MHz? it (equal (qxsl-band it)  10MHz))
(defun  14MHz? it (equal (qxsl-band it)  14MHz))
(defun  18MHz? it (equal (qxsl-band it)  18MHz))
(defun  21MHz? it (equal (qxsl-band it)  21MHz))
(defun  28MHz? it (equal (qxsl-band it)  28MHz))
(defun  50MHz? it (equal (qxsl-band it)  50MHz))
(defun 144MHz? it (equal (qxsl-band it) 144MHz))
(defun 430MHz? it (equal (qxsl-band it) 430MHz))
(defun 1.2GHz? it (equal (qxsl-band it) 1.2GHz))
(defun 2.4GHz? it (equal (qxsl-band it) 2.4GHz))
(defun 5.6GHz? it (equal (qxsl-band it) 5.6GHz))
(defun  10GHz? it (equal (qxsl-band it)  10GHz))

;; MODE FIELD ACCESS ROUTINES

; mode enumeration
(setq MORSE "(?i)CW")
(setq PHONE "(?i)PH|AM|FM|[DS]SB")
(setq DIGIT "(?i)DG|FT4|FT8|RTTY")

; mode access for cqww
(defun cqww-mode it
	(let mode (qxsl-mode it)
		(cond
			((equal mode "CW") "CW")
			((equal mode "PH") "SSB")
			((equal mode "DG") "RTTY"))))

; mode set for cqww
(defun set-cqww-mode (it val)
	(set-qxsl-mode it
		(cond
			((match val MORSE) "CW")
			((match val PHONE) "PH")
			((match val DIGIT) "DG"))))

; mode set for zlog
(defun set-zlog-mode (it val)
	(set-qxsl-mode it
		(if (match val DIGIT) "RTTY" val)))

; mode validation
(defun MORSE? it (match (qxsl-mode it) MORSE))
(defun PHONE? it (match (qxsl-mode it) PHONE))
(defun DIGIT? it (match (qxsl-mode it) DIGIT))
(defun CW/PH? it (or (MORSE? it) (PHONE? it)))
(defun AN/DG? it (or (CW/PH? it) (DIGIT? it)))

;; CODE FIELD ACCESS ROUTINES

; code access for zdos
(defun zdos-rstq it (car  (cut-rst-num it)))
(defun zdos-RSTQ it (car  (cut-RST-NUM it)))
(defun zdos-code it (cadr (cut-rst-num it)))
(defun zdos-CODE it (cadr (cut-RST-NUM it)))

; split code into RST and code
(defun reg-rst-num it (if (PHONE? it) "(?<=^..)" "(?<=^...)"))
(defun cut-rst-num it (split (qxsl-code it) (reg-rst-num it)))
(defun cut-RST-NUM it (split (qxsl-CODE it) (reg-rst-num it)))

; concatenate RST and code
(defun join-code it (format "%s%s" (qxsl-rstq it) (qxsl-code it)))
(defun join-CODE it (format "%s%s" (qxsl-RSTQ it) (qxsl-CODE it)))

; name validation
(defun SinOp? it #t)
(defun MulOp? it (not (member (qxsl-name it) (list null ""))))

;; FORMAT CONVERSION ROUTINES

; main routines
(defun transform (it fmt)
	(cond
		((equal fmt "qxml") (transform-qxsl it))
		((equal fmt "adxs") (transform-adif it))
		((equal fmt "adis") (transform-adif it))
		((equal fmt "cqww") (transform-cqww it))
		((equal fmt "jarl") (transform-jarl it))
		((equal fmt "ctxt") (transform-ctxt it))
		((equal fmt "zall") (transform-zlog it))
		((equal fmt "zdos") (transform-zdos it))
		((equal fmt "cbin") (transform-zdos it))
		((equal fmt "zbin") (transform-zlog it))))

(defun normalize (it fmt)
	(cond
		((adif? it) (normalize-adif it))
		((cqww? it) (normalize-cqww it))
		((qxsl? it) (normalize-qxsl it))
		((zdos? it) (normalize-zdos it))))

; format identification
(defun adif? it (not (null? (adif-TIME it))))
(defun qxsl? it (not (null? (qxsl-rstq it))))
(defun zdos? it (not (every it adif? qxsl? cqww?)))
(defun cqww? it (match (qxsl-mode it) "(?i)PH|DG"))

; field conversion into qxsl
(defun transform-qxsl it
	(let new (item)
		(set-qxsl-time new (qxsl-time it))
		(set-qxsl-call new (qxsl-call it))
		(set-qxsl-band new (qxsl-band it))
		(set-qxsl-mode new (qxsl-mode it))
		(set-qxsl-name new (qxsl-name it))
		(set-qxsl-rstq new (qxsl-rstq it))
		(set-qxsl-RSTQ new (qxsl-RSTQ it))
		(set-qxsl-code new (qxsl-code it))
		(set-qxsl-CODE new (qxsl-CODE it)) new))

; field conversion from qxsl
(defun normalize-qxsl it
	(let new (item)
		(set-qxsl-time new (qxsl-time it))
		(set-qxsl-call new (qxsl-call it))
		(set-qxsl-band new (qxsl-band it))
		(set-qxsl-mode new (qxsl-mode it))
		(set-qxsl-name new (qxsl-name it))
		(set-qxsl-rstq new (qxsl-rstq it))
		(set-qxsl-RSTQ new (qxsl-RSTQ it))
		(set-qxsl-code new (qxsl-code it))
		(set-qxsl-CODE new (qxsl-CODE it)) new))

; field conversion into adif
(defun transform-adif it
	(let new (item)
		(set-adif-time new (qxsl-time it))
		(set-adif-call new (qxsl-call it))
		(set-adif-band new (qxsl-band it))
		(set-adif-mode new (qxsl-mode it))
		(set-adif-name new (qxsl-name it))
		(set-adif-rstq new (qxsl-rstq it))
		(set-adif-RSTQ new (qxsl-RSTQ it))
		(set-adif-code new (qxsl-code it))
		(set-adif-CODE new (qxsl-CODE it)) new))

; field conversion from adif
(defun normalize-adif it
	(let new (item)
		(set-qxsl-time new (adif-time it))
		(set-qxsl-call new (adif-call it))
		(set-qxsl-band new (adif-band it))
		(set-qxsl-mode new (adif-mode it))
		(set-qxsl-name new (adif-name it))
		(set-qxsl-rstq new (adif-rstq it))
		(set-qxsl-RSTQ new (adif-RSTQ it))
		(set-qxsl-code new (adif-code it))
		(set-qxsl-CODE new (adif-CODE it)) new))

; field conversion into cqww
(defun transform-cqww it
	(let new (transform-qxsl it)
		(set-qxsl-name new null)
		(set-cqww-band new (qxsl-band it))
		(set-cqww-mode new (qxsl-mode it)) new))

; field conversion from cqww
(defun normalize-cqww it
	(let new (normalize-qxsl it)
		(set-qxsl-mode new (cqww-mode it)) new))

; field conversion into jarl
(defun transform-jarl it
	(let new (transform-qxsl it)
		(set-qxsl-name new null) new))

; field conversion into ctxt
(defun transform-ctxt it
	(let new (transform-zdos it)
		(set-qxsl-name new null) new))

; field conversion into zlog
(defun transform-zlog it
	(let new (transform-qxsl it)
		(set-zlog-mode new (qxsl-mode it)) new))

; field conversion into zdos
(defun transform-zdos it
	(let new (transform-qxsl it)
		(set-qxsl-rstq new null)
		(set-qxsl-RSTQ new null)
		(set-qxsl-code new (join-code it))
		(set-qxsl-CODE new (join-CODE it)) new))

; field conversion from zdos
(defun normalize-zdos it
	(let new (normalize-qxsl it)
		(set-qxsl-rstq new (zdos-rstq it))
		(set-qxsl-RSTQ new (zdos-RSTQ it))
		(set-qxsl-code new (zdos-code it))
		(set-qxsl-CODE new (zdos-CODE it)) new))
