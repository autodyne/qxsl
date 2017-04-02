(set 'setq (syntax (name value) (list 'set (list 'quote name) value)))
(setq defun (syntax (name pars body) (list 'setq name (list 'lambda pars body))))

(defun AM? (it) (member (hour it) (list 09 10 11)))
(defun PM? (it) (member (hour it) (list 16 17 18 19)))

(defun CW? (it) (member (mode it) (list "cw" "CW")))

(defun trim (text n) (if (> n 0) (trim (tail text) (- n 1)) text))
(defun CODE (it) (if (equal null (rstq (rcvd it))) (trim (code (rcvd it)) (if (CW? it) 3 2)) (code (rcvd it))))

(defun CITY (it) (city (CODE it)))
(defun PREF (it) (pref (CODE it)))

(defun PREF? (it) (equal (CITY it) (PREF it)))
(defun 支庁? (it) (member (CODE it) (list "101" "102" "103" "104" "105" "106" "107" "108" "109" "110" "111" "112" "113" "114")))
(defun AREA1? (it) (member (PREF it) (list "東京都" "神奈川県" "埼玉県" "千葉県" "群馬県" "茨城県" "栃木県" "山梨県")))
(defun AREA8? (it) (equal (PREF it) "北海道"))

(defun 内? (it) (if (AREA1? it) (not (PREF? it)) (if (AREA8? it) (支庁? it) (PREF? it))))
(defun 外? (it) (and (AREA1? it) (not (PREF? it))))

(defun 1.9MHz? (it) (and (PM? it) (<=  1900 (band it) 1913)))
(defun 3.5MHz? (it) (and (PM? it) (<=  3500 (band it) 3687)))
(defun   7MHz? (it) (and (PM? it) (<=  7000 (band it) 7200)))
                                           
(defun  14MHz? (it) (and (AM? it) (<= 14000 (band it) 14350)))
(defun  21MHz? (it) (and (AM? it) (<= 21000 (band it) 21450)))
(defun  28MHz? (it) (and (AM? it) (<= 28000 (band it) 29700)))
(defun  50MHz? (it) (and (AM? it) (<= 50000 (band it) 54000)))

(defun 低周波? (it) (or (1.9MHz? it) (3.5MHz? it) ( 7MHz? it)))
(defun 高周波? (it) (or ( 14MHz? it) ( 21MHz? it) (28MHz? it) (50MHz? it)))
(defun 全周波? (it) (or (低周波? it) (高周波? it)))

(defun BAND (it)
	(cond
		((1.9MHz? it)  1900)
		((3.5MHz? it)  3500)
		((  7MHz? it)  7000)
		(( 14MHz? it) 14000)
		(( 21MHz? it) 21000)
		(( 28MHz? it) 28000)
		(( 50MHz? it) 50000)))

(defun CALL-KEY (it) (list (call it) (BAND it) (mode it)))
(defun MULT-KEY (it) (list (BAND it) (CODE it)))
(defun 有効 (it) (list (CALL-KEY it) (MULT-KEY it) 1))

(contest "ALLJA1 TEST"
	(section "1エリア内 個人 電信限定 1.9MHz部門"         (lambda (it) (if (and (内? it) (CW? it) (1.9MHz? it)) (有効 it) "無効")))
	(section "1エリア内 個人 電信限定 3.5MHz部門"         (lambda (it) (if (and (内? it) (CW? it) (3.5MHz? it)) (有効 it) "無効")))
	(section "1エリア内 個人 電信限定 7MHz部門"           (lambda (it) (if (and (内? it) (CW? it) (  7MHz? it)) (有効 it) "無効")))
	(section "1エリア内 個人 電信電話 3.5MHz部門"         (lambda (it) (if (and (内? it)          (3.5MHz? it)) (有効 it) "無効")))
	(section "1エリア内 個人 電信電話 7MHz部門"           (lambda (it) (if (and (内? it)          (  7MHz? it)) (有効 it) "無効")))
	(section "1エリア内 個人 電信限定 1.9/3.5/7MHz部門"   (lambda (it) (if (and (内? it) (CW? it) (低周波? it)) (有効 it) "無効")))
	(section "1エリア内 個人 電信電話 1.9/3.5/7MHz部門"   (lambda (it) (if (and (内? it)          (低周波? it)) (有効 it) "無効")))

	(section "1エリア外 個人 電信限定 1.9MHz部門"         (lambda (it) (if (and (外? it) (CW? it) (1.9MHz? it)) (有効 it) "無効")))
	(section "1エリア外 個人 電信限定 3.5MHz部門"         (lambda (it) (if (and (外? it) (CW? it) (3.5MHz? it)) (有効 it) "無効")))
	(section "1エリア外 個人 電信限定 7MHz部門"           (lambda (it) (if (and (外? it) (CW? it) (  7MHz? it)) (有効 it) "無効")))
	(section "1エリア外 個人 電信電話 3.5MHz部門"         (lambda (it) (if (and (外? it)          (3.5MHz? it)) (有効 it) "無効")))
	(section "1エリア外 個人 電信電話 7MHz部門"           (lambda (it) (if (and (外? it)          (  7MHz? it)) (有効 it) "無効")))
	(section "1エリア外 個人 電信限定 1.9/3.5/7MHz部門"   (lambda (it) (if (and (外? it) (CW? it) (低周波? it)) (有効 it) "無効")))
	(section "1エリア外 個人 電信電話 1.9/3.5/7MHz部門"   (lambda (it) (if (and (外? it)          (低周波? it)) (有効 it) "無効")))

	(section "1エリア内 個人 電信限定 14MHz部門"          (lambda (it) (if (and (内? it) (CW? it) ( 14MHz? it)) (有効 it) "無効")))
	(section "1エリア内 個人 電信限定 21MHz部門"          (lambda (it) (if (and (内? it) (CW? it) ( 21MHz? it)) (有効 it) "無効")))
	(section "1エリア内 個人 電信限定 28MHz部門"          (lambda (it) (if (and (内? it) (CW? it) ( 28MHz? it)) (有効 it) "無効")))
	(section "1エリア内 個人 電信限定 50MHz部門"          (lambda (it) (if (and (内? it) (CW? it) ( 50MHz? it)) (有効 it) "無効")))
	(section "1エリア内 個人 電信電話 14MHz部門"          (lambda (it) (if (and (内? it)          ( 14MHz? it)) (有効 it) "無効")))
	(section "1エリア内 個人 電信電話 21MHz部門"          (lambda (it) (if (and (内? it)          ( 21MHz? it)) (有効 it) "無効")))
	(section "1エリア内 個人 電信電話 28MHz部門"          (lambda (it) (if (and (内? it)          ( 28MHz? it)) (有効 it) "無効")))
	(section "1エリア内 個人 電信電話 50MHz部門"          (lambda (it) (if (and (内? it)          ( 50MHz? it)) (有効 it) "無効")))
	(section "1エリア内 個人 電信限定 14/21/28/50MHz部門" (lambda (it) (if (and (内? it) (CW? it) (高周波? it)) (有効 it) "無効")))
	(section "1エリア内 個人 電信電話 14/21/28/50MHz部門" (lambda (it) (if (and (内? it)          (高周波? it)) (有効 it) "無効")))

	(section "1エリア外 個人 電信限定 14MHz部門"          (lambda (it) (if (and (外? it) (CW? it) ( 14MHz? it)) (有効 it) "無効")))
	(section "1エリア外 個人 電信限定 21MHz部門"          (lambda (it) (if (and (外? it) (CW? it) ( 21MHz? it)) (有効 it) "無効")))
	(section "1エリア外 個人 電信限定 28MHz部門"          (lambda (it) (if (and (外? it) (CW? it) ( 28MHz? it)) (有効 it) "無効")))
	(section "1エリア外 個人 電信限定 50MHz部門"          (lambda (it) (if (and (外? it) (CW? it) ( 50MHz? it)) (有効 it) "無効")))
	(section "1エリア外 個人 電信電話 14MHz部門"          (lambda (it) (if (and (外? it)          ( 14MHz? it)) (有効 it) "無効")))
	(section "1エリア外 個人 電信電話 21MHz部門"          (lambda (it) (if (and (外? it)          ( 21MHz? it)) (有効 it) "無効")))
	(section "1エリア外 個人 電信電話 28MHz部門"          (lambda (it) (if (and (外? it)          ( 28MHz? it)) (有効 it) "無効")))
	(section "1エリア外 個人 電信電話 50MHz部門"          (lambda (it) (if (and (外? it)          ( 50MHz? it)) (有効 it) "無効")))
	(section "1エリア外 個人 電信限定 14/21/28/50MHz部門" (lambda (it) (if (and (外? it) (CW? it) (高周波? it)) (有効 it) "無効")))
	(section "1エリア外 個人 電信電話 14/21/28/50MHz部門" (lambda (it) (if (and (外? it)          (高周波? it)) (有効 it) "無効")))

	(section "1エリア内 個人 電信限定 総合部門"           (lambda (it) (if (and (内? it) (CW? it) (全周波? it)) (有効 it) "無効")))
	(section "1エリア内 個人 電信電話 総合部門"           (lambda (it) (if (and (内? it)          (全周波? it)) (有効 it) "無効")))
	(section "1エリア内 社団 電信限定 オールバンド部門"   (lambda (it) (if (and (内? it) (CW? it) (全周波? it)) (有効 it) "無効")))
	(section "1エリア内 社団 電信電話 オールバンド部門"   (lambda (it) (if (and (内? it)          (全周波? it)) (有効 it) "無効")))

	(section "1エリア外 個人 電信限定 総合部門"           (lambda (it) (if (and (外? it) (CW? it) (全周波? it)) (有効 it) "無効")))
	(section "1エリア外 個人 電信電話 総合部門"           (lambda (it) (if (and (外? it)          (全周波? it)) (有効 it) "無効")))
	(section "1エリア外 社団 電信限定 オールバンド部門"   (lambda (it) (if (and (外? it) (CW? it) (全周波? it)) (有効 it) "無効")))
	(section "1エリア外 社団 電信電話 オールバンド部門"   (lambda (it) (if (and (外? it)          (全周波? it)) (有効 it) "無効"))))
