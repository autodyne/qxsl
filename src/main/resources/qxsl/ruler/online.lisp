;; REAL-TIME CONTEST DEFINED by 無線部開発班

(load "qxsl/ruler/jautil.lisp")

; band validation
(defun band? it (some it 3.5MHz? 7MHz? 14MHz? 21MHz? 28MHz? 50MHz?))

; time validation
(defun time? it (<= 13 (時刻 it) 15))

; area validation
(defun area? it
	(cond
		((AREA8? it) (every it 現存? 支庁?))
		((always it) (every it 現存? 府県?))))

; contact validation
(defmacro score conds
	`(verify ,conds (lambda it 1)))

; keys for identification
(defun unique it
	(list
		(qxsl-call it)
		(qxsl-band it)))

; keys for multiplication
(defun entity it
	(list
		(list
			(qxsl-band it)
			(qxsl-code it))))

; scoring
(defmacro result (score mults)
	`(* ,score (length (quote ,mults))))

; section names
(setq 個 "個人")
(setq 団 "団体")

(setq 電信 "電信限定")
(setq 電話 "電話限定")
(setq 電電 "電信電話")

; contest definition
(setq NAME "リアルタイムコンテスト")
(setq HOST "東大無線部")
(setq MAIL "allja1@ja1zlo.u-tokyo.org")
(setq LINK "ja1zlo.u-tokyo.org/rt")

; contest schedule
(defun startday year (schedule year 2 4 "SATURDAY"))
(defun finalday year (schedule year 2 4 "SATURDAY"))
(defun deadline year (schedule year 2 4 "SATURDAY"))
(setq RT (contest NAME HOST MAIL LINK startday finalday deadline))

; section macros
(defun rule s ((method 'add Contest Section) RT s))
(defmacro label args `(format "%s %s 部門" ,@args))
(defmacro build (n c a p) `(section ,n ,c ,a ,p unique entity result))
(defmacro SinOp (n c a p) `(rule (build (label ,n) ,c ,a (score ,p))))
(defmacro MulOp (n c a p) `(rule (build (label ,n) ,c ,a (score ,p))))

; prefectures only
(setq cities (remove-if-not (city-match "\\d{2}") CITY-LIST))

(SinOp (個 電信) 電信 cities (SinOp? band? time? area? MORSE?))
(SinOp (個 電話) 電話 cities (SinOp? band? time? area? PHONE?))
(SinOp (個 電電) 電電 cities (SinOp? band? time? area? CW/PH?))
(MulOp (団 電信) 電信 cities (MulOp? band? time? area? MORSE?))
(MulOp (団 電話) 電話 cities (MulOp? band? time? area? PHONE?))
(MulOp (団 電電) 電電 cities (MulOp? band? time? area? CW/PH?))
