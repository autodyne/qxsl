;; UTILITY MACROS DEFINED by 無線部開発班

; (defun name parameters body)
(set 'defun (syntax (name pars body)
	`(set ',name (lambda ,pars ,body))))

; (defmacro name parameters body)
(set 'defmacro (syntax (name pars body)
	`(set ',name (syntax ,pars ,body))))

; substring from head to tail (exclusive)
(defun substring (str head tail)
	(concat "" (subseq (split "" str) head tail)))

; map each argument to fun(argument)
(defun mapcar (fun args)
	(if
		(not (nil? args))
		(cons
			(fun (car args))
			(mapcar fun (cdr args)))))

; (dolist (argument arguments) body)
(defmacro dolist (args body)
	`(mapcar (lambda ,(car args) ,body) ,(cadr args)))

; (cond ((condition value)...))
(defmacro cond conds
	(if (not (nil? conds))
		`(if ,@(car conds) (cond ,(cdr conds)))))

; check if all conditions are satisfied for it
(defmacro forall (it conds)
	`(and ,@(dolist (c conds) `(,c it))))

; extract conditions unsatisfied for it
(defmacro unsat (it conds)
	(if (not (nil? conds))
		(let (head (car conds))
			`(if (,head it) (unsat it ,(cdr conds)) ',head))))
