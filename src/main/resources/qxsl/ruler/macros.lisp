;; UTILITY MACROS DEFINED by 無線部開発班

; define
(set 'defun (syntax (name pars body)
	`(set ',name (lambda ,pars ,body))))

(set 'defmacro (syntax (name pars body)
	`(set ',name (syntax ,pars ,body))))

; setq
(defmacro setq (name value) `(set ',name ,value))

; cadr/cdar
(defmacro cadr sexp `(car (cdr ,sexp)))
(defmacro cdar sexp `(cdr (car ,sexp)))

; nil?
(defmacro nil? sexp `(equal nil ,sexp))

; mapping routines
(defun mapcar (fun args)
	(if
		(not (nil? args))
		(cons
			(fun (car args))
			(mapcar fun (cdr args)))))

(defmacro dolist (args body)
	`(mapcar (lambda ,(car args) ,body) ,(cadr args)))

; scope
(defmacro let (binds body)
	`(progn ,@(dolist (ls binds) `(setq ,@ls)) ,body))

; conditional macros
(defmacro cond conds
	(if (not (nil? conds))
		`(if
			,(car (car conds))
			,(car (cdar conds))
			(cond ,(cdr conds)))))

(defmacro forall (it conds)
	`(every (list ,@(dolist (c conds) `(,c it)))))

(defmacro unsat (it conds)
	(if (not (nil? conds))
		(progn
			(setq head (car conds))
			(setq tail (cdr conds))
			`(if (,head it) (unsat it ,tail) ',head))))
