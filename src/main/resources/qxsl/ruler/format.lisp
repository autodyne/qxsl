;; ADIF-QXSL CONVERTER DEFINED by 無線部開発班

(load "qxsl/ruler/macros.lisp")

; XML namespaces
(setq qxsl "qxsl.org")
(setq adif "adif.org")

; field access for qxsl.org
(defun qxsl-time it (get-field it qxsl "time"))
(defun qxsl-call it (get-field it qxsl "call"))
(defun qxsl-mode it (get-field it qxsl "mode"))
(defun qxsl-name it (get-field it qxsl "name"))
(defun qxsl-rstq it (get-field (rcvd it) qxsl "rstq"))
(defun qxsl-code it (get-field (rcvd it) qxsl "code"))
(defun qxsl-RSTQ it (get-field (sent it) qxsl "rstq"))
(defun qxsl-CODE it (get-field (sent it) qxsl "code"))

; field access for adif.org
(defun adif-DATE it (get-field it adif "QSO_DATE"))
(defun adif-TIME it (get-field it adif "TIME_ON"))
(defun adif-call it (get-field it adif "CALL"))
(defun adif-mode it (get-field it adif "MODE"))
(defun adif-name it (get-field it adif "OPERATOR"))
(defun adif-rstq it (get-field it adif "RST_RCVD"))
(defun adif-code it (get-field it adif "SRX"))
(defun adif-RSTQ it (get-field it adif "RST_SENT"))
(defun adif-CODE it (get-field it adif "STX"))

; time access for adif.org
(defun adif-Y it (substring (adif-DATE it) 0 4))
(defun adif-M it (substring (adif-DATE it) 4 6))
(defun adif-D it (substring (adif-DATE it) 6 8))
(defun adif-h it (substring (adif-TIME it) 0 2))
(defun adif-m it (substring (adif-TIME it) 2 4))
(defun adif-s it (substring (adif-TIME it) 4 6))

; time conversion to ISO datetime format
(defun adif-dISO it (format "%s-%s-%s" (adif-Y it) (adif-M it) (adif-D it)))
(defun adif-tISO it (format "%s:%s:%s" (adif-h it) (adif-m it) (adif-s it)))
(defun adif-time it (format "%sT%sZ" (adif-dISO it) (adif-tISO it)))

; field set for qxsl.org
(defun set-qxsl-time (it value) (set-field it qxsl "time" value))
(defun set-qxsl-call (it value) (set-field it qxsl "call" value))
(defun set-qxsl-band (it value) (set-field it qxsl "band" value))
(defun set-qxsl-mode (it value) (set-field it qxsl "mode" value))
(defun set-qxsl-name (it value) (set-field it qxsl "name" value))
(defun set-qxsl-rstq (it value) (set-field (rcvd it) qxsl "rstq" value))
(defun set-qxsl-code (it value) (set-field (rcvd it) qxsl "code" value))
(defun set-qxsl-RSTQ (it value) (set-field (sent it) qxsl "rstq" value))
(defun set-qxsl-CODE (it value) (set-field (sent it) qxsl "code" value))

; ADIF/QXML checker
(defun qxsl? it (null? (adif-TIME it)))

; band access for qxsl.org
(defun qxsl-band it
	(let (freq (get-field it qxsl "band"))
		(cond (
			((<=  1800 freq  1913)  1900)
			((<=  3500 freq  3687)  3500)
			((<=  7000 freq  7200)  7000)
			((<= 14000 freq 14350) 14000)
			((<= 21000 freq 21450) 21000)
			((<= 28000 freq 29700) 28000)
			((<= 50000 freq 54000) 50000)))))

; band access for adif.org
(defun adif-band it
	(let (band (get-field it adif "BAND"))
		(cond (
			((equal band "160m")  "1900")
			((equal band  "80m")  "3500")
			((equal band  "40m")  "7000")
			((equal band  "20m") "14000")
			((equal band  "15m") "21000")
			((equal band  "10m") "28000")
			((equal band   "6m") "50000")))))

; field copy from ADIF to QXSL
(defun toQXSL it
	(if
		(not (qxsl? it))
		(progn
			(set-qxsl-time it (adif-time it))
			(set-qxsl-call it (adif-call it))
			(set-qxsl-band it (adif-band it))
			(set-qxsl-mode it (adif-mode it))
			(set-qxsl-name it (adif-name it))
			(set-qxsl-rstq it (adif-rstq it))
			(set-qxsl-code it (adif-code it))
			(set-qxsl-RSTQ it (adif-RSTQ it))
			(set-qxsl-CODE it (adif-CODE it)))))
(handler "toQXSL" (lambda it (progn (toQXSL it) it)))
