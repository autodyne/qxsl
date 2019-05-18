;; ElvaLisp function & special-forms Tests

; quote
'(1 2 3) (list 1 2 3)
`(1 2 3) (list 1 2 3)

; unquote
`(3 ,(+ 3 3) 4) '(3 6 4)
`(1 (2 ,(* 3 4)) 5) '(1 (2 12) 5)

; progn
(progn 11) 11
(progn 11 45) 45
(progn 11 45 14) 14

; set
(set 'foo 13) 13
(progn (set 'foo 29) (set 'bar 97) foo) 29
(progn (set 'foo 29) (set 'bar 97) bar) 97

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

; empty?
(empty? ()) true
(empty? (list 'HEAD)) false
(empty? (list 'HEAD 'TAIL)) false
(empty? (list 'HEAD 'NEXT 'TAIL)) false

; length
(length ()) 0
(length (list 'HEAD)) 1
(length (list 'HEAD 'TAIL)) 2
(length (list 'HEAD 'NEXT 'TAIL)) 3

; member
(member 'HEAD ()) false
(member 'HEAD (list 'HEAD)) true
(member 'HEAD (list 'HEAD 'TAIL)) true
(member 'HEAD (list 'HEAD 'NEXT 'TAIL)) true
(member 'NEXT (list 'HEAD)) false
(member 'NEXT (list 'HEAD 'TAIL)) false
(member 'NEXT (list 'HEAD 'NEXT 'TAIL)) true
(member 'TAIL (list 'HEAD)) false
(member 'TAIL (list 'HEAD 'TAIL)) true
(member 'TAIL (list 'HEAD 'NEXT 'TAIL)) true

; mapcar
(mapcar (lambda (x) (* x x)) '(1 2 3 4 5)) '(1 4 9 16 25)

; equal
(equal 1 1) true
(equal 1 2) false
(equal "HEAD" "HEAD") true
(equal "HEAD" "TAIL") false
(equal (+ 1 2) (+ 3 0)) true
(equal (+ 4 5) (+ 6 7)) false

; if
(if true 'HEAD 'TAIL) 'HEAD
(if false 'HEAD 'TAIL) 'TAIL
(if (equal 28 28) (+ 114 514) (+ 364 364)) 628
(if (equal 28 29) (+ 114 514) (+ 364 364)) 728

; and
(and (equal 364 364) (equal 364 364)) true
(and (equal 364 364) (equal 114 514)) false
(and (equal 114 514) (equal 114 514)) false
(and (equal 114 514) (equal 364 364)) false

; or
(or (equal 364 364) (equal 364 364)) true
(or (equal 364 364) (equal 114 514)) true
(or (equal 114 514) (equal 114 514)) false
(or (equal 114 514) (equal 364 364)) true

; not
(not (equal 114 514)) true
(not (not (equal 114 514))) false
(not (not (not (equal 114 514)))) true

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
(/ 364364 1919) 189
(/ 889464 1919) 463

; <
(< 114 115) true
(< 115 115) false
(< 115 114) false
(< 1 2 3 4 5) true
(< 1 2 3 2 5) false

; >
(> 114 115) false
(> 115 115) false
(> 115 114) true
(> 5 4 3 2 1) true
(> 5 4 2 3 1) false

; <=
(<= 114 115) true
(<= 115 115) true
(<= 115 114) false
(<= 1 2 3 4 5) true
(<= 1 2 3 2 5) false

; >=
(>= 114 115) false
(>= 115 115) true
(>= 115 114) true
(>= 5 4 3 2 1) true
(>= 5 4 2 3 1) false

; str-head
(str-head "HELLO") "H"
(str-head "WORLD") "W"

; str-tail
(str-tail "HELLO") "ELLO"
(str-tail "WORLD") "ORLD"

; match
(match "\\d{6}" (if (equal 114 514) "114514" "364364")) true
(match "\\D{6}" (if (equal 114 514) "114514" "364364")) false

; lambda
((lambda x (+ x x)) 114) 228
((lambda (x) (* x x)) 114) 12996
((lambda (x y) (* x y)) 114 514) 58596

; syntax
(progn (set 'setq (syntax (n v) `(set ',n ,v))) (setq MUR "KMR") MUR) "KMR"
