;; ElvaLisp function & special-forms Tests

; quote
'(1 2 3) (list 1 2 3)

; quasi-quote
`(1 2 3) (list 1 2 3)

; unquote
`(3 ,(+ 3 3) 4) '(3 6 4)
`(1 (2 ,(* 3 4)) 5) '(1 (2 12) 5)

; unquote-splicing
`(1 (1 4 ,@(cdr '(1 5 1 4)))) '(1 (1 4 5 1 4))

; block
(block 11) 11
(block 11 45) 45
(block 11 45 14) 14
(block (+ 364 364)) 728

; set
(set 'foo 13) 13
(block (set 'foo 29) (set 'bar 97) foo) 29
(block (set 'foo 29) (set 'bar 97) bar) 97

; let
(let foo 810 foo) 810
(let bar 364 bar (+ bar bar)) 728
(let bar 364 bar (+ bar bar) (* bar bar)) 132496

; symbol
(symbol "MUR") 'MUR
(symbol "KMR") 'KMR

; list
(list) '()
(list 1) '(1)
(list 3 7) '(3 7)
(list 8 (list 1 0)) '(8 (1 0))
(list 3 (list 6) 4) '(3 (6) 4)
(list 'HEAD 'TAIL) '(HEAD TAIL)

; cons
(cons 1 ()) (list 1)
(cons 1 '(2)) (list 1 2)
(cons 2 (list 3)) (list 2 3)
(cons 4 (list 5 6)) (list 4 5 6)

; car
(car (list 'HEAD)) 'HEAD
(car (list 'HEAD 'TAIL)) 'HEAD
(car (list 'HEAD 'NEXT 'TAIL)) 'HEAD

; cdr
(cdr '(1 2)) '(2)
(cdr (list 'HEAD)) ()
(cdr (list 'HEAD 'TAIL)) (list 'TAIL)
(cdr (list 'HEAD 'NEXT 'TAIL)) (list 'NEXT 'TAIL)

; cadr
(cadr (list 'HEAD 'TAIL)) 'TAIL
(cadr (list 'HEAD 'NEXT 'TAIL)) 'NEXT

; cddr
(cddr (list 'HEAD 'TAIL)) ()
(cddr (list 'HEAD 'NEXT 'TAIL)) (list 'TAIL)

; nth
(nth 0 (list 11 4 5 14)) 11
(nth 3 (list 11 4 5 14)) 14

; subseq
(subseq (list 11 4 5 14) 1 3) '(4 5)
(subseq (list 11 4 5 14) 0 4) '(11 4 5 14)

; length
(length ()) 0
(length (list 'HEAD)) 1
(length (list 'HEAD 'TAIL)) 2
(length (list 'HEAD 'NEXT 'TAIL)) 3

; member
(member 'HEAD ()) #f
(member 'HEAD (list 'HEAD)) #t
(member 'HEAD (list 'HEAD 'TAIL)) #t
(member 'HEAD (list 'HEAD 'NEXT 'TAIL)) #t
(member 'NEXT (list 'HEAD)) #f
(member 'NEXT (list 'HEAD 'TAIL)) #f
(member 'NEXT (list 'HEAD 'NEXT 'TAIL)) #t
(member 'TAIL (list 'HEAD)) #f
(member 'TAIL (list 'HEAD 'TAIL)) #t
(member 'TAIL (list 'HEAD 'NEXT 'TAIL)) #t

; nil?
(nil? ()) #t
(nil? 810) #f
(nil? null) #f
(nil? (car (list null))) #f
(nil? (cdr (list null))) #t

; null?
(null? ()) #f
(null? 810) #f
(null? null) #t
(null? (car (list null null))) #t
(null? (cdr (list null null))) #f

; equal
(equal 1 1) #t
(equal 1 2) #f
(equal "HEAD" "HEAD") #t
(equal "HEAD" "TAIL") #f
(equal (+ 1 2) (+ 3 0)) #t
(equal (+ 4 5) (+ 6 7)) #f

; if
(if #t 'HEAD 'TAIL) 'HEAD
(if #f 'HEAD 'TAIL) 'TAIL
(if #t (+ 114 514)) 628
(if #f (* 114 514)) nil
(if (equal 28 28) (+ 114 514) (+ 364 364)) 628
(if (equal 28 29) (+ 114 514) (+ 364 364)) 728

; and
(and (equal 364 364) (equal 364 364)) #t
(and (equal 364 364) (equal 114 514)) #f
(and (equal 114 514) (equal 114 514)) #f
(and (equal 114 514) (equal 364 364)) #f

; or
(or (equal 364 364) (equal 364 364)) #t
(or (equal 364 364) (equal 114 514)) #t
(or (equal 114 514) (equal 114 514)) #f
(or (equal 114 514) (equal 364 364)) #t

; xor
(xor (equal 364 364) (equal 364 364)) #f
(xor (equal 364 364) (equal 114 514)) #t
(xor (equal 114 514) (equal 114 514)) #f
(xor (equal 114 514) (equal 364 364)) #t

; not
(not (equal 114 514)) #t
(not (not (equal 114 514))) #f
(not (not (not (equal 114 514)))) #t

; +
(+ 28 28) 56
(+ 1 1 4 (+ 5 1 4)) 16
(+ (+ 3 6 4) 3 6 4) 26

; -
(- 28 28) 0
(- 1 1 4 (- 5 1 4)) -4
(- (+ 8 8 9) 4 6 4) 11

; *
(* 28 28) 784
(* 3 6 4 (* 3 6 4)) 5184
(* (+ 8 8 9) 4 6 4) 2400

; /
(/ 28 28) 1
(/ 364364 1919) 189.8718082334549
(/ 889464 1919) 463.5039082855654

; mod
(mod 28 28) 0
(mod 364364 1919) 1673
(mod 889464 1919)  967

; <
(< 114 115) #t
(< 115 115) #f
(< 115 114) #f
(< 1 2 3 4 5) #t
(< 1 2 3 2 5) #f

; >
(> 114 115) #f
(> 115 115) #f
(> 115 114) #t
(> 5 4 3 2 1) #t
(> 5 4 2 3 1) #f

; <=
(<= 114 115) #t
(<= 115 115) #t
(<= 115 114) #f
(<= 1 2 3 4 5) #t
(<= 1 2 3 2 5) #f

; >=
(>= 114 115) #f
(>= 115 115) #t
(>= 115 114) #t
(>= 5 4 3 2 1) #t
(>= 5 4 2 3 1) #f

; format
(format "%s %s" 114 (+ 200 314)) "114 514"
(format "%s %s" (+ 182 182) 364) "364 364"

; lambda
((lambda x (+ x x)) 114) 228
((lambda (x) (* x x)) 114) 12996
((lambda (x y) (* x y)) 114 514) 58596

; syntax
(((syntax (pars body) `(lambda ,pars ,body)) (x y) (+ x y)) 114 514) 628
(((syntax (pars body) `(syntax ,pars ,body)) (x y) (+ x y)) 364 364) 728

; type
(type "TNOK") java.lang.String
(type 114514) java.lang.Integer

; method
((method 'length java.lang.String) "foobar") 6
((method 'getSimpleName java.lang.Class) java.lang.String) "String"
((method 'getSimpleName java.lang.Class) java.lang.Object) "Object"

; assert
(catch (assert (equal 3 (+ 1 2)) "must be true")) null
(catch (assert (equal 3 (+ 3 2)) "must be true")) "must be true"

; catch
; throw
(catch (block 3 6 4 (throw "YATAI") 3)) "YATAI"
(catch (block 8 1 0 (throw "RAMEN") 3)) "RAMEN"

; eval
(eval 10) 10
(eval '(+ 10 20)) 30
(block (set 'x 100) (eval x)) 100

; load
(block (load "qxsl/ruler/common.lisp") (defun foo x (+ x x)) (foo 114)) 228
(block (load "qxsl/ruler/common.lisp") (defun bar y (+ y y)) (bar 364)) 728
