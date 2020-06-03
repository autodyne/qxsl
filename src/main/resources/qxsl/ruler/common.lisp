;; COMMON LIBRARY DEFINED by 無線部開発班

; setq
(set 'setq (syntax (name value)
	`(set ',name ,value)))

(assert (equal (block (setq foo "FOO") foo) "FOO") "setq")
(assert (equal (block (setq bar "BAR") bar) "BAR") "setq")


; defun
(setq defun (syntax (name pars body)
	`(set ',name (lambda ,pars ,body))))

(assert (equal (block (defun add (x y) (+ x y)) (add 114 514)) 628) "defun")
(assert (equal (block (defun add (x y) (+ x y)) (add 364 364)) 728) "defun")


; defmacro
(setq defmacro (syntax (name pars body)
	`(set ',name (syntax ,pars ,body))))

(assert (equal (block (defmacro = (x y) `(setq ,x ,y)) (= a 7) a) 7) "defmacro")


; mapcar
(defun mapcar (fun args)
	(if
		(not (nil? args))
		(cons
			(fun (car args))
			(mapcar fun (cdr args)))))

(assert (equal (mapcar (lambda x (+ x 1)) (list 1 2 3)) (list 2 3 4)) "mapcar")
(assert (equal (mapcar (lambda x (* x 2)) (list 1 2 3)) (list 2 4 6)) "mapcar")


; dolist
(defmacro dolist (args body)
	`(mapcar (lambda ,(car args) ,body) ,(cadr args)))

(assert (equal (dolist (i (list 1 2 3)) (+ i 3)) (list 4 5 6)) "dolist")
(assert (equal (dolist (i (list 1 2 3)) (* i 2)) (list 2 4 6)) "dolist")


; cond
(defmacro cond conds
	(if (not (nil? conds))
		`(if ,@(car conds) (cond ,(cdr conds)))))

(assert (equal (let (x 1) (cond (((equal x 1) "A") ((equal x 2) "B")))) "A") "cond")
(assert (equal (let (x 2) (cond (((equal x 1) "A") ((equal x 2) "B")))) "B") "cond")


; every
(defmacro every (it conds)
	`(and ,@(dolist (c conds) `(,c ,it))))

(assert (equal (every 2 ((lambda y (> y 1)) (lambda z (< z 3)))) #t) "every")
(assert (equal (every 3 ((lambda y (> y 1)) (lambda z (< z 3)))) #f) "every")


; search
(defmacro search (it conds)
	(if (not (nil? conds))
		(let (head (car conds))
			`(if (,head ,it) (search ,it ,(cdr conds)) ',head))))

(assert (not (nil? (search 5 ((lambda y (> y 1)) (lambda z (< z 3)))))) "search")
(assert (not (nil? (search 0 ((lambda y (> y 1)) (lambda z (< z 3)))))) "search")


; import
(defmacro import class
	`(set (symbol ((access java.lang.Class 'getSimpleName) ,class)) ,class))

(assert (equal (block (import java.lang.String) String) java.lang.String) "import")
(assert (equal (block (import java.lang.Object) Object) java.lang.Object) "import")


; . (member function)
(defmacro . (method obj args)
	`((access (type ,obj) ',method) ,obj ,@args))

(assert (equal (. toUpperCase "kmr" ()) "KMR") ".")
(assert (equal (. toLowerCase "MUR" ()) "mur") ".")


; : (static variable)
(defmacro : (access type)
	`((access ,type ',access) null))

(assert (equal (: MAX_VALUE java.lang.Integer)  2147483647) ":")
(assert (equal (: MIN_VALUE java.lang.Integer) -2147483648) ":")


; string
(defun string obj
	((access java.util.Objects 'toString) null obj))

(assert (equal (string  114514) "114514") "string")
(assert (equal (string 'FOOBAR) "FOOBAR") "string")

(assert (equal (string (lambda x (+ x x))) "(lambda (x) (+ x x))") "string")
(assert (equal (string (syntax x (+ x x))) "(syntax (x) (+ x x))") "string")


; integer
(defun integer num
	(. intValue num ()))

(assert (equal (integer 0.0) 0) "integer")
(assert (equal (integer 0.1) 0) "integer")
(assert (equal (integer 1.0) 1) "integer")
(assert (equal (integer 1.2) 1) "integer")


; setScale
(setq setScale (access java.math.BigDecimal 'setScale))


; FLOOR
(setq FLOOR (: FLOOR java.math.RoundingMode))


; CEILING
(setq CEILING (: CEILING java.math.RoundingMode))


; HALF_UP
(setq HALF_UP (: HALF_UP java.math.RoundingMode))


; floor
(defun floor num
	(setScale num 0 FLOOR))

(assert (equal (floor (+  5 0.5))  5) "floor")
(assert (equal (floor (+  2 0.5))  2) "floor")
(assert (equal (floor (+  1 0.6))  1) "floor")
(assert (equal (floor (+  1 0.1))  1) "floor")
(assert (equal (floor (+  1 0.0))  1) "floor")
(assert (equal (floor (- -1 0.0)) -1) "floor")
(assert (equal (floor (- -1 0.1)) -2) "floor")
(assert (equal (floor (- -1 0.6)) -2) "floor")
(assert (equal (floor (- -2 0.5)) -3) "floor")
(assert (equal (floor (- -5 0.5)) -6) "floor")


; ceiling
(defun ceiling num
	(setScale num 0 CEILING))

(assert (equal (ceiling (+  5 0.5))  6) "ceiling")
(assert (equal (ceiling (+  2 0.5))  3) "ceiling")
(assert (equal (ceiling (+  1 0.6))  2) "ceiling")
(assert (equal (ceiling (+  1 0.1))  2) "ceiling")
(assert (equal (ceiling (+  1 0.0))  1) "ceiling")
(assert (equal (ceiling (- -1 0.0)) -1) "ceiling")
(assert (equal (ceiling (- -1 0.1)) -1) "ceiling")
(assert (equal (ceiling (- -1 0.6)) -1) "ceiling")
(assert (equal (ceiling (- -2 0.5)) -2) "ceiling")
(assert (equal (ceiling (- -5 0.5)) -5) "ceiling")


; round
(defun round num
	(setScale num 0 HALF_UP))

(assert (equal (round (+  5 0.5))  6) "round")
(assert (equal (round (+  2 0.5))  3) "round")
(assert (equal (round (+  1 0.6))  2) "round")
(assert (equal (round (+  1 0.1))  1) "round")
(assert (equal (round (+  1 0.0))  1) "round")
(assert (equal (round (- -1 0.0)) -1) "round")
(assert (equal (round (- -1 0.1)) -1) "round")
(assert (equal (round (- -1 0.6)) -2) "round")
(assert (equal (round (- -2 0.5)) -3) "round")
(assert (equal (round (- -5 0.5)) -6) "round")


; join
(setq join (access java.lang.String 'join))


; concatenate
(defun concatenate (del seq)
	(eval `(join null ,del ,@seq)))

(assert (equal (concatenate "-" (list "MUR" "KMR")) "MUR-KMR") "concatenate")
(assert (equal (concatenate "+" (list "364" "364")) "364+364") "concatenate")


; split
(setq split (access java.lang.String 'split))

(assert (equal (split "11:45::14" ":+") '("11" "45" "14")) "split")


; substring
(defun substring (text head tail)
	(concatenate "" (subseq (split text "") head tail)))

(assert (equal (substring "FOOBAR" 1 3) "OO") "substring")
(assert (equal (substring "FOOBAR" 3 5) "BA") "substring")


; match
(setq match (access java.lang.String 'matches))

(assert (equal (match (if (equal 114 514) "114514" "364364") "\\d{6}") #t) "match")
(assert (equal (match (if (equal 114 514) "114514" "364364") "\\D{6}") #f) "match")
