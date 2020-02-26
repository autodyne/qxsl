;; UTILITY MACROS DEFINED by 無線部開発班

; set/defun/defmacro
(set 'setq
	(syntax (name value)
		`(set ',name ,value)))

(setq defun
	(syntax (name pars body)
		`(setq ,name (lambda ,pars ,body))))

(setq defmacro
	(syntax (name pars body)
		`(setq ,name (syntax ,pars ,body))))

; cadr/cdar
(defmacro cadr sexp `(car (cdr ,sexp)))
(defmacro cdar sexp `(cdr (car ,sexp)))

; nil?
(defmacro nil? sexp `(equal nil ,sexp))

; mapping functions
(defun mapcar (fun args)
	(progn
		(defun map args
			(if
				(nil? args)
				nil
				(cons
					(fun (car args))
					(map (cdr args)))))
		(map args)))

(defmacro dolist (args fun)
	`(mapcar
		(lambda ,(car args) ,fun)
		,(cadr args)))

; conditional macros
(defmacro cond conds
	(if
		(nil? conds)
		null
		`(if
			,(car (car conds))
			,(car (cdr (car conds)))
			(cond ,(cdr conds)))))

(defmacro forall (it conds)
	`(every (list ,@(dolist (c conds) (list c it)))))
