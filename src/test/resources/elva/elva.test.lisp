;; ElvaLisp function & special-forms Tests

; quote
'(1 2 3) (list 1 2 3)
`(1 2 3) (list 1 2 3)

; unquote
`(3 ,(+ 3 3) 4) '(3 6 4)
`(1 (2 ,(* 3 4)) 5) '(1 (2 12) 5)
`(1 (1 4 ,@(cdr '(1 5 1 4)))) '(1 (1 4 5 1 4))

; progn
(progn 11) 11
(progn 11 45) 45
(progn 11 45 14) 14

; set
(set 'foo 13) 13
(progn (set 'foo 29) (set 'bar 97) foo) 29
(progn (set 'foo 29) (set 'bar 97) bar) 97

; eval
(eval 10) 10
(eval '(+ 10 20)) 30
(progn (set 'x 100) (eval x)) 100

; cons
(cons 1 ()) (list 1)
(cons 2 (list 3 4)) (list 2 3 4)

; list
(list) '()
(list 1) '(1)
(list 3 7) '(3 7)
(list 8 1 0) '(8 1 0)
(list 3 6 4) '(3 6 4)

; car
(car (list 'HEAD)) 'HEAD
(car (list 'HEAD 'TAIL)) 'HEAD
(car (list 'HEAD 'NEXT 'TAIL)) 'HEAD

; cdr
(cdr (list 'HEAD)) ()
(cdr (list 'HEAD 'TAIL)) (list 'TAIL)
(cdr (list 'HEAD 'NEXT 'TAIL)) (list 'NEXT 'TAIL)

; nth
(nth 0 (list 11 4 5 14)) 11
(nth 3 (list 11 4 5 14)) 14

; subseq
(subseq (list 11 4 5 14) 1 3) '(4 5)
(subseq (list 11 4 5 14) 0 4) '(11 4 5 14)

; length
(number (length ())) 0
(number (length (list 'HEAD))) 1
(number (length (list 'HEAD 'TAIL))) 2
(number (length (list 'HEAD 'NEXT 'TAIL))) 3

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

; equal
(equal 1 1) #t
(equal 1 2) #f
(equal "HEAD" "HEAD") #t
(equal "HEAD" "TAIL") #f
(equal (+ 1 2) (+ 3 0)) #t
(equal (+ 4 5) (+ 6 7)) #f

; null?
(null? 810) #f
(null? null) #t
(null? (car (list null null))) #t
(null? (cdr (list null null))) #f

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

; ceiling
(ceiling (+  5 0.5))  6
(ceiling (+  2 0.5))  3
(ceiling (+  1 0.6))  2
(ceiling (+  1 0.1))  2
(ceiling (+  1 0.0))  1
(ceiling (- -1 0.0)) -1
(ceiling (- -1 0.1)) -1
(ceiling (- -1 0.6)) -1
(ceiling (- -2 0.5)) -2
(ceiling (- -5 0.5)) -5

; floor
(floor (+  5 0.5))  5
(floor (+  2 0.5))  2
(floor (+  1 0.6))  1
(floor (+  1 0.1))  1
(floor (+  1 0.0))  1
(floor (- -1 0.0)) -1
(floor (- -1 0.1)) -2
(floor (- -1 0.6)) -2
(floor (- -2 0.5)) -3
(floor (- -5 0.5)) -6

; round
(round (+  5 0.5))  6
(round (+  2 0.5))  3
(round (+  1 0.6))  2
(round (+  1 0.1))  1
(round (+  1 0.0))  1
(round (- -1 0.0)) -1
(round (- -1 0.1)) -1
(round (- -1 0.6)) -2
(round (- -2 0.5)) -3
(round (- -5 0.5)) -6

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

; concat
(concat (nth 0 '("snpi" "-")) (list "MUR" "KMR")) "MURsnpiKMR"
(concat (nth 1 '("snpi" "-")) (list '(11 4) 514)) "(11 4)-514"

; format
(format "%s %s" "MUR" "KMR") "MUR KMR"
(format "%s %s" 114 (+ 200 314)) "114 514"

; match
(match "\\d{6}" (if (equal 114 514) "114514" "364364")) #t
(match "\\D{6}" (if (equal 114 514) "114514" "364364")) #f

; split
(split "" "ABC") '("A" "B" "C")
(split ":+" "11:45::14") '("11" "45" "14")

; number
(number "114.514") 114.514
(number "1919810") 1919810

; string
(string +) "+"
(string -) "-"
(string "ABCDE") "ABCDE"
(string 514.114) "514.114"
(string 8101919) "8101919"
(string (lambda x (+ x x))) "(lambda (x) (+ x x))"
(string (syntax x (+ x x))) "(syntax (x) (+ x x))"

; lambda
((lambda x (+ x x)) 114) 228
((lambda (x) (* x x)) 114) 12996
((lambda (x y) (* x y)) 114 514) 58596

; syntax
(progn (set 'setq (syntax (n v) `(set ',n ,v))) (setq MUR "KMR") MUR) "KMR"

; load
(progn (load "qxsl/ruler/macros.lisp") (defun foo x (+ x x)) (foo 114)) 228
