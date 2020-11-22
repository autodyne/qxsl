;; ALLJA1 CONTEST DEFINED by 無線部開発班

(load "qxsl/ruler/jautil.lisp")

; band validation
(defun bHI? it (some it  14MHz?  21MHz? 28MHz?  50MHz?))
(defun bLO? it (some it 1.9MHz? 3.5MHz?  7MHz?))
(defun bDG? it (some it   7MHz?))

; time validation
(defun tHI? it (and (<= 09 (時刻 it) 11) (CW/PH? it) (bHI? it)))
(defun tDG? it (and (<= 13 (時刻 it) 14) (DIGIT? it) (bDG? it)))
(defun tLO? it (and (<= 16 (時刻 it) 19) (CW/PH? it) (bLO? it)))
(defun tAN? it (some it tHI? tLO?))
(defun tJS? it (some it tHI? tLO? tDG?))

;; area validation
(defun INNER? it
	(and
		(現存? it)
		(cond
			((DIGIT? it) (市郡? it))
			((AREA1? it) (市郡? it))
			((AREA8? it) (支庁? it))
			((always it) (府県? it)))))

(defun OUTER? it (every it 現存? AREA1? 市郡?))

; contact validation
(defmacro score conds
	`(verify ,conds (lambda it 1)))

; keys for identification
(defun unique it
	(list
		(qxsl-call it)
		(qxsl-band it)
		(cond
			((MORSE? it) 1)
			((PHONE? it) 2)
			((DIGIT? it) 3))))

; keys for multiplication
(defun entity it
	(list
		(qxsl-band it)
		(qxsl-code it)))

; entity for SinOP
(defun EntitySinOp it
	(list (entity it) null))

; entity for MulOP
(defun EntityMulOp it
	(list (entity it) (qxsl-name it)))

; scoring
(defmacro result (score mults names)
	`(block
		(setq mults (length (quote ,mults)))
		(setq names (length (quote ,names)))
		(ceiling (/ (* ,score mults) names))))

; section names
(setq 内 "1エリア内")
(setq 外 "1エリア外")

(setq 個 "個人")
(setq 団 "団体")

(setq 電信 "電信限定")
(setq 電電 "電信電話")
(setq 離散 "デジタル")
(setq 総合 "総合")

(setq L19部門   "1.9MHz")
(setq L35部門   "3.5MHz")
(setq L70部門     "7MHz")
(setq H14部門    "14MHz")
(setq H21部門    "21MHz")
(setq H28部門    "28MHz")
(setq H50部門    "50MHz")
(setq LAB部門 "1.9-7MHz")
(setq HAB部門 "14-50MHz")
(setq ALL部門 "")

; contest definition
(setq NAME "ALLJA1")
(setq HOST "東大無線部")
(setq MAIL "allja1@ja1zlo.u-tokyo.org")
(setq LINK "ja1zlo.u-tokyo.org/allja1")

; contest schedule
(defun startday year (schedule year 6 4 "SATURDAY"))
(defun finalday year (schedule year 6 4 "SATURDAY"))
(defun deadline year (schedule year 7 3 "SATURDAY"))
(setq JA1 (contest NAME HOST MAIL LINK startday finalday deadline))

; section macros
(defun rule s ((method! 'add) JA1 s))
(defmacro label args `(format "%s %s %s %s部門" ,@args))
(defmacro build (n c p m) `(rule (section ,n ,c ,p unique ,m result)))
(defmacro SinOp (n c p) `(build (label ,n) ,c (score ,p) EntitySinOp))
(defmacro MulOp (n c p) `(build (label ,n) ,c (score ,p) EntityMulOp))

; section codes
(defmacro SinHB (name test) `(SinOp ,name "個人部門 (HB)" ,test))
(defmacro SinLB (name test) `(SinOp ,name "個人部門 (LB)" ,test))
(defmacro SinDG (name test) `(SinOp ,name "個人部門 (DG)" ,test))
(defmacro SinJS (name test) `(SinOp ,name "個人部門 (JS)" ,test))
(defmacro MulAB (name test) `(MulOp ,name "団体部門 (AB)" ,test))
(defmacro MulDG (name test) `(MulOp ,name "団体部門 (DG)" ,test))
(defmacro MulJS (name test) `(MulOp ,name "団体部門 (JS)" ,test))

; 1エリア内 個人 電信限定 ローバンド部門
(SinLB (内 個 電信 L19部門) (SinOp? INNER? tLO? MORSE? 1.9MHz?))
(SinLB (内 個 電信 L35部門) (SinOp? INNER? tLO? MORSE? 3.5MHz?))
(SinLB (内 個 電信 L70部門) (SinOp? INNER? tLO? MORSE?   7MHz?))
(SinLB (内 個 電信 LAB部門) (SinOp? INNER? tLO? MORSE?))

; 1エリア内 個人 電信電話 ローバンド部門
(SinLB (内 個 電電 L35部門) (SinOp? INNER? tLO? CW/PH? 3.5MHz?))
(SinLB (内 個 電電 L70部門) (SinOp? INNER? tLO? CW/PH?   7MHz?))
(SinLB (内 個 電電 LAB部門) (SinOp? INNER? tLO? CW/PH?))

; 1エリア外 個人 電信限定 ローバンド部門
(SinLB (外 個 電信 L19部門) (SinOp? OUTER? tLO? MORSE? 1.9MHz?))
(SinLB (外 個 電信 L35部門) (SinOp? OUTER? tLO? MORSE? 3.5MHz?))
(SinLB (外 個 電信 L70部門) (SinOp? OUTER? tLO? MORSE?   7MHz?))
(SinLB (外 個 電信 LAB部門) (SinOp? OUTER? tLO? MORSE?))

; 1エリア外 個人 電信電話 ローバンド部門
(SinLB (外 個 電電 L35部門) (SinOp? OUTER? tLO? CW/PH? 3.5MHz?))
(SinLB (外 個 電電 L70部門) (SinOp? OUTER? tLO? CW/PH?   7MHz?))
(SinLB (外 個 電電 LAB部門) (SinOp? OUTER? tLO? CW/PH?))

; 1エリア内 個人 電信限定 ハイバンド部門
(SinHB (内 個 電信 H14部門) (SinOp? INNER? tHI? MORSE?  14MHz?))
(SinHB (内 個 電信 H21部門) (SinOp? INNER? tHI? MORSE?  21MHz?))
(SinHB (内 個 電信 H28部門) (SinOp? INNER? tHI? MORSE?  28MHz?))
(SinHB (内 個 電信 H50部門) (SinOp? INNER? tHI? MORSE?  50MHz?))
(SinHB (内 個 電信 HAB部門) (SinOp? INNER? tHI? MORSE?))

; 1エリア内 個人 電信電話 ハイバンド部門
(SinHB (内 個 電電 H14部門) (SinOp? INNER? tHI? CW/PH?  14MHz?))
(SinHB (内 個 電電 H21部門) (SinOp? INNER? tHI? CW/PH?  21MHz?))
(SinHB (内 個 電電 H28部門) (SinOp? INNER? tHI? CW/PH?  28MHz?))
(SinHB (内 個 電電 H50部門) (SinOp? INNER? tHI? CW/PH?  50MHz?))
(SinHB (内 個 電電 HAB部門) (SinOp? INNER? tHI? CW/PH?))

; 1エリア外 個人 電信限定 ハイバンド部門
(SinHB (外 個 電信 H14部門) (SinOp? OUTER? tHI? MORSE?  14MHz?))
(SinHB (外 個 電信 H21部門) (SinOp? OUTER? tHI? MORSE?  21MHz?))
(SinHB (外 個 電信 H28部門) (SinOp? OUTER? tHI? MORSE?  28MHz?))
(SinHB (外 個 電信 H50部門) (SinOp? OUTER? tHI? MORSE?  50MHz?))
(SinHB (外 個 電信 HAB部門) (SinOp? OUTER? tHI? MORSE?))

; 1エリア外 個人 電信電話 ハイバンド部門
(SinHB (外 個 電電 H14部門) (SinOp? OUTER? tHI? CW/PH?  14MHz?))
(SinHB (外 個 電電 H21部門) (SinOp? OUTER? tHI? CW/PH?  21MHz?))
(SinHB (外 個 電電 H28部門) (SinOp? OUTER? tHI? CW/PH?  28MHz?))
(SinHB (外 個 電電 H50部門) (SinOp? OUTER? tHI? CW/PH?  50MHz?))
(SinHB (外 個 電電 HAB部門) (SinOp? OUTER? tHI? CW/PH?))

; 1エリア内 団体 アナログ部門
(MulAB (内 団 電信 ALL部門) (MulOp? INNER? tAN? MORSE?))
(MulAB (内 団 電電 ALL部門) (MulOp? INNER? tAN? CW/PH?))

; 1エリア外 団体 アナログ部門
(MulAB (外 団 電信 ALL部門) (MulOp? OUTER? tAN? MORSE?))
(MulAB (外 団 電電 ALL部門) (MulOp? OUTER? tAN? CW/PH?))

; 個人 デジタル部門
(SinDG (内 個 離散 ALL部門) (SinOp? INNER? tDG? DIGIT?))
(SinDG (外 個 離散 ALL部門) (SinOp? OUTER? tDG? DIGIT?))

; 団体 デジタル部門
(MulDG (内 団 離散 ALL部門) (MulOp? INNER? tDG? DIGIT?))
(MulDG (外 団 離散 ALL部門) (MulOp? OUTER? tDG? DIGIT?))

; 個人 総合部門
(SinJS (内 個 総合 ALL部門) (SinOp? INNER? tJS? AN/DG?))
(SinJS (外 個 総合 ALL部門) (SinOp? OUTER? tJS? AN/DG?))

; 団体 総合部門
(MulJS (内 団 総合 ALL部門) (MulOp? INNER? tJS? AN/DG?))
(MulJS (外 団 総合 ALL部門) (MulOp? OUTER? tJS? AN/DG?))
