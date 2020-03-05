;; UTILITY MACROS DEFINED by 無線部開発班

; (defun name parameters body)
(set 'defun (syntax (name pars body)
	`(set ',name (lambda ,pars ,body))))

; (defmacro name parameters body)
(set 'defmacro (syntax (name pars body)
	`(set ',name (syntax ,pars ,body))))

; (setq name value)
(defmacro setq (name value) `(set ',name ,value))

; equivalent (car (cdr list))
(defmacro cadr sexp `(car (cdr ,sexp)))

; equivalent (cdr (car list))
(defmacro cdar sexp `(cdr (car ,sexp)))

; check if sexp is an empty list
(defmacro nil? sexp `(equal nil ,sexp))

; substring from head to tail (exclusive)
(defun substring (str head tail)
	(concat (subseq (split "" str) head tail)))

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

; (let ((variable-name variable-value)...) body)
(defmacro let (binds body)
	`(progn ,@(dolist (ls binds) `(setq ,@ls)) ,body))

; remove duplicates from a sorted list
(defun remove-duplicates ls
	(if (not (nil? ls))
		(let ((tail (remove-duplicates (cdr ls))))
			(if (equal (car ls) (cadr ls))
				(cons (car ls) tail)
				tail))))

; (cond ((condition value)...))
(defmacro cond conds
	(if (not (nil? conds))
		`(if
			,(car (car conds))
			,(car (cdar conds))
			(cond ,(cdr conds)))))

; check if all conditions are satisfied for it
(defmacro forall (it conds)
	`(every (list ,@(dolist (c conds) `(,c it)))))

; extract conditions unsatisfied for it
(defmacro unsat (it conds)
	(if (not (nil? conds))
		(progn
			(setq head (car conds))
			(setq tail (cdr conds))
			`(if (,head it) (unsat it ,tail) ',head))))
