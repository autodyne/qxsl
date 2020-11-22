(lambda x ());(lambda x ());false;1;1
(lambda y 12);(lambda y 12);false;1;1
(lambda z "");(lambda z "");false;1;1
(lambda (a) "");(lambda (a) "");false;1;1
(lambda (b) #t);(lambda (b) #t);false;1;1
(lambda (c) #f);(lambda (c) #f);false;1;1
(lambda (a b) "");(lambda (a b) "");false;2;2
(lambda (c d) #t);(lambda (c d) #t);false;2;2
(lambda (e f) #f);(lambda (e f) #f);false;2;2
(lambda (a b c) 114);(lambda (a b c) 114);false;3;3
(lambda (d e f) 514);(lambda (d e f) 514);false;3;3
(lambda *x ());(lambda *x ());true;0;256
(lambda *y 12);(lambda *y 12);true;0;256
(lambda *z "");(lambda *z "");true;0;256
(lambda (*a) "");(lambda (*a) "");true;0;256
(lambda (*b) #t);(lambda (*b) #t);true;0;256
(lambda (*c) #f);(lambda (*c) #f);true;0;256
(lambda (a *b) "");(lambda (a *b) "");true;1;256
(lambda (c *d) #t);(lambda (c *d) #t);true;1;256
(lambda (e *f) #f);(lambda (e *f) #f);true;1;256
(lambda (a b *c) 114);(lambda (a b *c) 114);true;2;256
(lambda (d e *f) 514);(lambda (d e *f) 514);true;2;256
