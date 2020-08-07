;; FORMAT CONVERTER DEFINED by 無線部開発班

;; SUPPORTED FORMATS:
;; qxsl: qxml, cqww, jarl, ctxt, zall, zdos, cbin, zbin
;; adif: adis, adxs

(load "qxsl/ruler/common.lisp")

; XML namespaces
(setq qxsl "qxsl.org")
(setq adif "adif.org")

;; GENERAL FIELD ACCESS ROUTINES

; field keys for qxsl.org
(setq key-qxsl-time "time")
(setq key-qxsl-call "call")
(setq key-qxsl-band "band")
(setq key-qxsl-mode "mode")
(setq key-qxsl-name "name")
(setq key-qxsl-rstq "rstq")
(setq key-qxsl-code "code")

; field keys for adif.org
(setq key-adif-DATE "QSO_DATE")
(setq key-adif-TIME "TIME_ON")
(setq key-adif-call "CALL")
(setq key-adif-band "BAND")
(setq key-adif-mode "MODE")
(setq key-adif-name "OPERATOR")
(setq key-adif-rstq "RST_RCVD")
(setq key-adif-RSTQ "RST_SENT")
(setq key-adif-code "SRX")
(setq key-adif-CODE "STX")

; field access for qxsl.org
(defun qxsl-time it (get-field it qxsl key-qxsl-time))
(defun qxsl-call it (get-field it qxsl key-qxsl-call))
(defun qxsl-BAND it (get-field it qxsl key-qxsl-band))
(defun qxsl-mode it (get-field it qxsl key-qxsl-mode))
(defun qxsl-name it (get-field it qxsl key-qxsl-name))
(defun qxsl-rstq it (get-field (rcvd it) qxsl key-qxsl-rstq))
(defun qxsl-RSTQ it (get-field (sent it) qxsl key-qxsl-rstq))
(defun qxsl-code it (get-field (rcvd it) qxsl key-qxsl-code))
(defun qxsl-CODE it (get-field (sent it) qxsl key-qxsl-code))

; field access for adif.org
(defun adif-DATE it (get-field it adif key-adif-DATE))
(defun adif-TIME it (get-field it adif key-adif-TIME))
(defun adif-call it (get-field it adif key-adif-call))
(defun adif-BAND it (get-field it adif key-adif-band))
(defun adif-mode it (get-field it adif key-adif-mode))
(defun adif-name it (get-field it adif key-adif-name))
(defun adif-rstq it (get-field it adif key-adif-rstq))
(defun adif-RSTQ it (get-field it adif key-adif-RSTQ))
(defun adif-code it (get-field it adif key-adif-code))
(defun adif-CODE it (get-field it adif key-adif-CODE))

; field set for qxsl.org
(defun set-qxsl-time (it val) (set-field it qxsl key-qxsl-time val))
(defun set-qxsl-call (it val) (set-field it qxsl key-qxsl-call val))
(defun set-qxsl-band (it val) (set-field it qxsl key-qxsl-band val))
(defun set-qxsl-mode (it val) (set-field it qxsl key-qxsl-mode val))
(defun set-qxsl-name (it val) (set-field it qxsl key-qxsl-name val))
(defun set-qxsl-rstq (it val) (set-field (rcvd it) qxsl key-qxsl-rstq val))
(defun set-qxsl-RSTQ (it val) (set-field (sent it) qxsl key-qxsl-rstq val))
(defun set-qxsl-code (it val) (set-field (rcvd it) qxsl key-qxsl-code val))
(defun set-qxsl-CODE (it val) (set-field (sent it) qxsl key-qxsl-code val))

; field set for adif.org
(defun set-adif-DATE (it val) (set-field it adif key-adif-DATE it))
(defun set-adif-TIME (it val) (set-field it adif key-adif-TIME it))
(defun set-adif-call (it val) (set-field it adif key-adif-call it))
(defun set-adif-BAND (it val) (set-field it adif key-adif-band it))
(defun set-adif-mode (it val) (set-field it adif key-adif-mode it))
(defun set-adif-name (it val) (set-field it adif key-adif-name it))
(defun set-adif-rstq (it val) (set-field it adif key-adif-rstq it))
(defun set-adif-RSTQ (it val) (set-field it adif key-adif-RSTQ it))
(defun set-adif-code (it val) (set-field it adif key-adif-code it))
(defun set-adif-CODE (it val) (set-field it adif key-adif-CODE it))

;; TIME FIELD ACCESS ROUTINES

; time access for adif.org
(defun adif-dISO it (replace (adif-DATE it) "(?<=^.{4}|^.{6})" "-"))
(defun adif-tISO it (replace (adif-TIME it) "(?<=^.{2}|^.{4})" ":"))
(defun adif-time it (format "%sT%sZ" (adif-dISO it) (adif-tISO it)))

; time set for adif.org
(defun set-adif-time (it val)
	(block
		(set-adif-DATE it (replace (substring (qxsl-time it)  0 10) "-" ""))
		(set-adif-TIME it (replace (substring (qxsl-time it) 11 19) ":" ""))))

;; BAND FIELD ACCESS ROUTINES

; band enumeration for qxsl.org
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

; band enumeration for adif.org
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

; band access for qxsl.org
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

; band access for adif.org
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

; band set for adif.org
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

; mode validation
(defun Morse? it (match (qxsl-mode it) "(?i)CW"))
(defun Phone? it (match (qxsl-mode it) "(?i)PH|AM|FM|[DS]SB"))
(defun Digit? it (match (qxsl-mode it) "(?i)DG|FT4|FT8|RTTY"))

;; CODE FIELD ACCESS ROUTINES

; remove RST from code
(defun peel-code it
	(if
		(null? (qxsl-rstq it))
		(replace (qxsl-code it) (if (Phone? it) "^.." "^...") "")
		(qxsl-code it)))

; concatenate RST and code
(defun join-code it (concatenate "" (list (qxsl-rstq it) (qxsl-code it))))
(defun join-CODE it (concatenate "" (list (qxsl-RSTQ it) (qxsl-CODE it))))

;; FORMAT CONVERSION ROUTINES

(defun adif? it (not (null? (adif-TIME it))))

; field conversion into qxsl.org
(defun toQXSL it
	(if (adif? it)
		(block
			(catch (set-qxsl-time it (adif-time it)))
			(catch (set-qxsl-call it (adif-call it)))
			(catch (set-qxsl-band it (adif-band it)))
			(catch (set-qxsl-mode it (adif-mode it)))
			(catch (set-qxsl-name it (adif-name it)))
			(catch (set-qxsl-rstq it (adif-rstq it)))
			(catch (set-qxsl-RSTQ it (adif-RSTQ it)))
			(catch (set-qxsl-code it (adif-code it)))
			(catch (set-qxsl-CODE it (adif-CODE it))))))

; field conversion into adif.org
(defun toADIF it
	(block
		(catch (set-adif-time it (qxsl-time it)))
		(catch (set-adif-call it (qxsl-call it)))
		(catch (set-adif-band it (qxsl-band it)))
		(catch (set-adif-mode it (qxsl-mode it)))
		(catch (set-adif-name it (qxsl-name it)))
		(catch (set-adif-rstq it (qxsl-rstq it)))
		(catch (set-adif-RSTQ it (qxsl-RSTQ it)))
		(catch (set-adif-code it (qxsl-code it)))
		(catch (set-adif-CODE it (qxsl-CODE it)))))

; field conversion into ZDOS (or CTXT)
(defun toZDOS it
	(block
		(toQXSL it)
		(catch (set-qxsl-rstq it null))
		(catch (set-qxsl-RSTQ it null))
		(catch (set-qxsl-code it (join-code)))
		(catch (set-qxsl-CODE it (join-CODE)))))

; reformat item into format whose name specified
(defun reformat (it fmt)
	(cond
		((equal fmt "qxml") (toQXSL it))
		((equal fmt "adxs") (toADIF it))
		((equal fmt "adis") (toADIF it))
		((equal fmt "cqww") (toQXSL it))
		((equal fmt "jarl") (toQXSL it))
		((equal fmt "ctxt") (toZDOS it))
		((equal fmt "zall") (toQXSL it))
		((equal fmt "zdos") (toZDOS it))
		((equal fmt "cbin") (toZDOS it))
		((equal fmt "zbin") (toQXSL it))))
