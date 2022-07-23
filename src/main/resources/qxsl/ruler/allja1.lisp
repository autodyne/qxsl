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

; area validation
(defun 内? it
	(cond
		((DIGIT? it) (every it 現存? 市郡?))
		((AREA1? it) (every it 現存? 市郡?))
		((AREA8? it) (every it 現存? 支庁?))
		((always it) (every it 現存? 府県?))))

(defun 外? it (every it 現存? AREA1? 市郡?))

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
(defmacro build (n c a p m) `(rule (section ,n ,c ,a ,p unique ,m result)))
(defmacro SinOp (n c a p) `(build (label ,n) ,c ,a (score ,p) EntitySinOp))
(defmacro MulOp (n c a p) `(build (label ,n) ,c ,a (score ,p) EntityMulOp))

; section codes
(setq cSinHB "個人部門 09:00-12:00")
(setq cSinLB "個人部門 16:00-20:00")
(setq cSinDG "個人部門 13:00-15:00")
(setq cMulAB "団体部門 アナログ")
(setq cMulDG "団体部門 デジタル")
(setq cMixJS "総合部門")

; remove prefectures
(setq cities-ja1 (remove-if (city-match "\\d{2,3}") cities-area1))
(setq out-of-ja1 (remove-if (city-match "\\d{2,3}") out-of-area1))

; section constructors
(defmacro SinHB (name area test) `(SinOp ,name cSinHB ,area ,test))
(defmacro SinLB (name area test) `(SinOp ,name cSinLB ,area ,test))
(defmacro SinDG (name area test) `(SinOp ,name cSinDG ,area ,test))
(defmacro SinJS (name area test) `(SinOp ,name cMixJS ,area ,test))
(defmacro MulAB (name area test) `(MulOp ,name cMulAB ,area ,test))
(defmacro MulDG (name area test) `(MulOp ,name cMulDG ,area ,test))
(defmacro MulJS (name area test) `(MulOp ,name cMixJS ,area ,test))

; 不参加
(rule (absence (format "%s 不参加 (チェックログ)" cSinHB) cSinHB))
(rule (absence (format "%s 不参加 (チェックログ)" cSinLB) cSinLB))
(rule (absence (format "%s 不参加 (チェックログ)" cSinDG) cSinDG))
(rule (absence (format "%s 不参加 (チェックログ)" cMulAB) cMulAB))
(rule (absence (format "%s 不参加 (チェックログ)" cMulDG) cMulDG))
(rule (absence (format "%s 不参加 (チェックログ)" cMixJS) cMixJS))

; 1エリア内 個人 電信限定 ローバンド部門
(SinLB (内 個 電信 L19部門) cities-ja1 (SinOp? 内? tLO? MORSE? 1.9MHz?))
(SinLB (内 個 電信 L35部門) cities-ja1 (SinOp? 内? tLO? MORSE? 3.5MHz?))
(SinLB (内 個 電信 L70部門) cities-ja1 (SinOp? 内? tLO? MORSE?   7MHz?))
(SinLB (内 個 電信 LAB部門) cities-ja1 (SinOp? 内? tLO? MORSE?))

; 1エリア内 個人 電信電話 ローバンド部門
(SinLB (内 個 電電 L19部門) cities-ja1 (SinOp? 内? tLO? CW/PH? 1.9MHz?))
(SinLB (内 個 電電 L35部門) cities-ja1 (SinOp? 内? tLO? CW/PH? 3.5MHz?))
(SinLB (内 個 電電 L70部門) cities-ja1 (SinOp? 内? tLO? CW/PH?   7MHz?))
(SinLB (内 個 電電 LAB部門) cities-ja1 (SinOp? 内? tLO? CW/PH?))

; 1エリア外 個人 電信限定 ローバンド部門
(SinLB (外 個 電信 L19部門) out-of-ja1 (SinOp? 外? tLO? MORSE? 1.9MHz?))
(SinLB (外 個 電信 L35部門) out-of-ja1 (SinOp? 外? tLO? MORSE? 3.5MHz?))
(SinLB (外 個 電信 L70部門) out-of-ja1 (SinOp? 外? tLO? MORSE?   7MHz?))
(SinLB (外 個 電信 LAB部門) out-of-ja1 (SinOp? 外? tLO? MORSE?))

; 1エリア外 個人 電信電話 ローバンド部門
(SinLB (外 個 電電 L19部門) out-of-ja1 (SinOp? 外? tLO? CW/PH? 1.9MHz?))
(SinLB (外 個 電電 L35部門) out-of-ja1 (SinOp? 外? tLO? CW/PH? 3.5MHz?))
(SinLB (外 個 電電 L70部門) out-of-ja1 (SinOp? 外? tLO? CW/PH?   7MHz?))
(SinLB (外 個 電電 LAB部門) out-of-ja1 (SinOp? 外? tLO? CW/PH?))

; 1エリア内 個人 電信限定 ハイバンド部門
(SinHB (内 個 電信 H14部門) cities-ja1 (SinOp? 内? tHI? MORSE?  14MHz?))
(SinHB (内 個 電信 H21部門) cities-ja1 (SinOp? 内? tHI? MORSE?  21MHz?))
(SinHB (内 個 電信 H28部門) cities-ja1 (SinOp? 内? tHI? MORSE?  28MHz?))
(SinHB (内 個 電信 H50部門) cities-ja1 (SinOp? 内? tHI? MORSE?  50MHz?))
(SinHB (内 個 電信 HAB部門) cities-ja1 (SinOp? 内? tHI? MORSE?))

; 1エリア内 個人 電信電話 ハイバンド部門
(SinHB (内 個 電電 H14部門) cities-ja1 (SinOp? 内? tHI? CW/PH?  14MHz?))
(SinHB (内 個 電電 H21部門) cities-ja1 (SinOp? 内? tHI? CW/PH?  21MHz?))
(SinHB (内 個 電電 H28部門) cities-ja1 (SinOp? 内? tHI? CW/PH?  28MHz?))
(SinHB (内 個 電電 H50部門) cities-ja1 (SinOp? 内? tHI? CW/PH?  50MHz?))
(SinHB (内 個 電電 HAB部門) cities-ja1 (SinOp? 内? tHI? CW/PH?))

; 1エリア外 個人 電信限定 ハイバンド部門
(SinHB (外 個 電信 H14部門) out-of-ja1 (SinOp? 外? tHI? MORSE?  14MHz?))
(SinHB (外 個 電信 H21部門) out-of-ja1 (SinOp? 外? tHI? MORSE?  21MHz?))
(SinHB (外 個 電信 H28部門) out-of-ja1 (SinOp? 外? tHI? MORSE?  28MHz?))
(SinHB (外 個 電信 H50部門) out-of-ja1 (SinOp? 外? tHI? MORSE?  50MHz?))
(SinHB (外 個 電信 HAB部門) out-of-ja1 (SinOp? 外? tHI? MORSE?))

; 1エリア外 個人 電信電話 ハイバンド部門
(SinHB (外 個 電電 H14部門) out-of-ja1 (SinOp? 外? tHI? CW/PH?  14MHz?))
(SinHB (外 個 電電 H21部門) out-of-ja1 (SinOp? 外? tHI? CW/PH?  21MHz?))
(SinHB (外 個 電電 H28部門) out-of-ja1 (SinOp? 外? tHI? CW/PH?  28MHz?))
(SinHB (外 個 電電 H50部門) out-of-ja1 (SinOp? 外? tHI? CW/PH?  50MHz?))
(SinHB (外 個 電電 HAB部門) out-of-ja1 (SinOp? 外? tHI? CW/PH?))

; 1エリア内 団体 アナログ部門
(MulAB (内 団 電信 ALL部門) cities-ja1 (MulOp? 内? tAN? MORSE?))
(MulAB (内 団 電電 ALL部門) cities-ja1 (MulOp? 内? tAN? CW/PH?))

; 1エリア外 団体 アナログ部門
(MulAB (外 団 電信 ALL部門) out-of-ja1 (MulOp? 外? tAN? MORSE?))
(MulAB (外 団 電電 ALL部門) out-of-ja1 (MulOp? 外? tAN? CW/PH?))

; 個人 デジタル部門
(SinDG (内 個 離散 ALL部門) cities-ja1 (SinOp? 内? tDG? DIGIT?))
(SinDG (外 個 離散 ALL部門) out-of-ja1 (SinOp? 外? tDG? DIGIT?))

; 団体 デジタル部門
(MulDG (内 団 離散 ALL部門) cities-ja1 (MulOp? 内? tDG? DIGIT?))
(MulDG (外 団 離散 ALL部門) out-of-ja1 (MulOp? 外? tDG? DIGIT?))

; 個人 総合部門
(SinJS (内 個 総合 ALL部門) cities-ja1 (SinOp? 内? tJS? AN/DG?))
(SinJS (外 個 総合 ALL部門) out-of-ja1 (SinOp? 外? tJS? AN/DG?))

; 団体 総合部門
(MulJS (内 団 総合 ALL部門) cities-ja1 (MulOp? 内? tJS? AN/DG?))
(MulJS (外 団 総合 ALL部門) out-of-ja1 (MulOp? 外? tJS? AN/DG?))
