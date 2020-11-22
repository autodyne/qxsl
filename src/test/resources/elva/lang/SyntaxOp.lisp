(syntax x ());(syntax x ());false;1;1
(syntax y 12);(syntax y 12);false;1;1
(syntax z "");(syntax z "");false;1;1
(syntax (a) "");(syntax (a) "");false;1;1
(syntax (b) #t);(syntax (b) #t);false;1;1
(syntax (c) #f);(syntax (c) #f);false;1;1
(syntax (a b) "");(syntax (a b) "");false;2;2
(syntax (c d) #t);(syntax (c d) #t);false;2;2
(syntax (e f) #f);(syntax (e f) #f);false;2;2
(syntax (a b c) 114);(syntax (a b c) 114);false;3;3
(syntax (d e f) 514);(syntax (d e f) 514);false;3;3
(syntax *x ());(syntax *x ());true;0;256
(syntax *y 12);(syntax *y 12);true;0;256
(syntax *z "");(syntax *z "");true;0;256
(syntax (*a) "");(syntax (*a) "");true;0;256
(syntax (*b) #t);(syntax (*b) #t);true;0;256
(syntax (*c) #f);(syntax (*c) #f);true;0;256
(syntax (a *b) "");(syntax (a *b) "");true;1;256
(syntax (c *d) #t);(syntax (c *d) #t);true;1;256
(syntax (e *f) #f);(syntax (e *f) #f);true;1;256
(syntax (a b *c) 114);(syntax (a b *c) 114);true;2;256
(syntax (d e *f) 514);(syntax (d e *f) 514);true;2;256
