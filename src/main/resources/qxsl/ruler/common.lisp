;; COMMON LIBRARY DEFINED by 無線部開発班

(load "qxsl/ruler/import.lisp")

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
	(if (not (nil? args))
		(cons (fun (car args)) (mapcar fun (cdr args)))))
(assert (equal (mapcar (lambda x (+ x 1)) (list 1 2 3)) (list 2 3 4)) "mapcar")
(assert (equal (mapcar (lambda x (* x 2)) (list 1 2 3)) (list 2 4 6)) "mapcar")

; dolist
(defmacro dolist (args body)
	`(mapcar (lambda ,(car args) ,body) ,(cadr args)))
(assert (equal (dolist (i (list 1 2 3)) (+ i 3)) (list 4 5 6)) "dolist")
(assert (equal (dolist (i (list 1 2 3)) (* i 2)) (list 2 4 6)) "dolist")

; cond
(defmacro cond *conds
	(if (not (nil? *conds))
		`(if ,@(car *conds) (cond ,@(cdr *conds)))))
(assert (equal (let x 1 (cond ((equal x 1) "A") ((equal x 2) "B"))) "A") "cond")
(assert (equal (let x 2 (cond ((equal x 1) "A") ((equal x 2) "B"))) "B") "cond")

; always
(defun always *args #t)
(assert (always) "always")
(assert (always 1) "always")
(assert (always 1 2) "always")

; every
(defmacro every (it *conds) `(and ,@(dolist (c *conds) `(,c ,it))))
(assert (equal (every 2 (lambda y (> y 1)) (lambda z (< z 3))) #t) "every")
(assert (equal (every 3 (lambda y (> y 1)) (lambda z (< z 3))) #f) "every")

; some
(defmacro some (it *conds) `(or ,@(dolist (c *conds) `(,c ,it))))
(assert (equal (some 2 (lambda y (> y 1)) (lambda z (< z 3))) #t) "some")
(assert (equal (some 3 (lambda y (> y 1)) (lambda z (< z 3))) #t) "some")

; search
(defmacro search (it conds)
	(if (not (nil? conds))
		(let head (car conds)
			`(if (,head ,it) (search ,it ,(cdr conds)) ',head))))
(assert (not (nil? (search 5 ((lambda y (> y 1)) (lambda z (< z 3)))))) "search")
(assert (not (nil? (search 0 ((lambda y (> y 1)) (lambda z (< z 3)))))) "search")

; string
(defun string obj ((method 'toString Objects Object) obj))
(assert (equal (string  114514) "114514") "string")
(assert (equal (string 'FOOBAR) "FOOBAR") "string")
(assert (equal (string (lambda x (+ x x))) "(lambda x (+ x x))") "string")
(assert (equal (string (syntax x (+ x x))) "(syntax x (+ x x))") "string")

; boolean
(defun boolean str (equal "#t" str))
(assert (equal (boolean "#t") #t) "boolean")
(assert (equal (boolean "#f") #f) "boolean")

; integer
(defun integer num ((method 'intValue Number) num))
(assert (equal (integer 0.0) 0) "integer")
(assert (equal (integer 0.1) 0) "integer")
(assert (equal (integer 1.0) 1) "integer")
(assert (equal (integer 1.2) 1) "integer")

; setScale
(setq setScale (method 'setScale BigDecimal int RoundingMode))

; floor
(defun floor num (setScale num 0 ((access 'FLOOR RoundingMode))))
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
(defun ceiling num (setScale num 0 ((access 'CEILING RoundingMode))))
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
(defun round num (setScale num 0 ((access 'HALF_UP RoundingMode))))
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

; split
(setq split (method 'split String String))
(assert (equal (split "11:45::14" ":+") '("11" "45" "14")) "split")
(assert (equal (split "36^4%364" "\\W") '("36" "4" "364")) "split")

; substring
(defun substring (text head tail)
	((method 'substring String int int) text head tail))
(assert (equal (substring "FOOBAR" 1 3) "OO") "substring")
(assert (equal (substring "FOOBAR" 3 5) "BA") "substring")

; match
(setq match (method 'matches String String))
(assert (equal (match (if (equal 114 514) "114514" "364364") "\\d{6}") #t) "match")
(assert (equal (match (if (equal 114 514) "114514" "364364") "\\D{6}") #f) "match")

; replace
(setq replace (method 'replaceAll String String String))
(assert (equal (replace "114" "11|51" "36") "364") "replace")
(assert (equal (replace "514" "11|51" "36") "364") "replace")

; get hour value from ZonedDateTime
(setq hour (method 'getHour ZonedDateTime))

; get ZoneID of the specified name
(defun zone name ((method 'of ZoneId String) name))
(assert (not (null? (zone "Europe/Paris"))) "zone")

; get ZonedDateTime at another time zone
(setq at-zone (method 'withZoneSameInstant ZonedDateTime ZoneId))

; format time into an ISO-8601 string
(defun iso-8601 time
	((method 'format ZonedDateTime DateTimeFormatter)
		time ((access 'ISO_ZONED_DATE_TIME DateTimeFormatter))))

; contact validation
(defmacro verify (conds score)
	`(lambda it
		(let it (normalize it null)
			(let msg (search it ,conds)
				(if (nil? msg)
					(success it (,score it))
					(failure it msg))))))

; create item
(setq item (new Item))
(setq rcvd (method 'getRcvd Item))
(setq sent (method 'getSent Item))

; access field
(setq getf (method 'value Tuple QName))
(setq setf (method 'set Tuple QName Object))

; create success/failure
(setq success (new Success Item int))
(setq failure (new Failure Item Object))

; get code or name from the specified city
(setq code<-city (method 'code LocalCityItem))
(setq area<-city (method 'area LocalCityItem))
(setq name<-city (method 'name LocalCityItem int))

; get city from the specified code or name
(setq city<-code (method 'getByCode LocalCityBase String))
(setq city<-name (method 'getByName LocalCityBase String))

; contest schedule
(defun schedule (year month week day)
	(let day ((method! 'valueOf DayOfWeek) day)
		(let adj (method! 'dayOfWeekInMonth TemporalAdjusters)
			(let 1st ((method! 'of LocalDate) year month 1)
				((method! 'with LocalDate) 1st (adj week day))))))
