;;initial test of interpeter
 (%+ (%* 6 7) 8)
;;output 
50

(%cons (%quote (a b c)) (%quote (d e f)))
;;out
((a b c) d e f)

(%define (%factorial x)
	 (%if (%< x 1)
	      1
	     (%* x  (%factorial (%- x 1)))))

 (%factorial 4)
;;out 
24

(%define (%map fn lst) )


(%define (make-account balance )
  (%define (withdraw amount)
    (%if (%>= balance amount)
	(%begin (%set! balance (%- balance amount))
	       balance)
	"insufficient funds"))
  (%define (deposit amount)
    (%begin (%set! balance (%+ balance amount))
	   balance))
  (%define (dispatch m)
    (%cond ((%eq? m (%quote withdraw)) withdraw)
	((%eq? m (%quote deposit)) deposit)
	(%else (error "Unknown request --MAKE ACCOUNT--" m))))
  dispatch)
;;output
ok

(%define balance 0)
;;output
ok
(%define acc (make-account balance))
;;output 
ok

((acc (%quote deposit)) 100)
;;output 
100
;;some more simple expressions 

(init-%scheme)
%==> (%cons (%quote a) 2)
(a . 2)
%==> (%+ 5 (%* 3 4))
17
%==> (%define (%inc x) (%+ x 1))
ok
%==> (%inc 14)
15

;;assignment 2
;; map function here called mapping can be implemented as ordinary function 
;; using primitives of %scheme. Solution follows map as desctribed on page 
;; 105 sicp. There really nothing special about map so no need to change
;; anything in the interpeter (meta_eval, abssyntax, enviroment)
(%define (mapping proc items)
  (%if (%null? items)
       (%quote ())
      (%cons (proc (%car items)) 
	    (mapping proc (%cdr items)))))
;;testing it 
(mapping (%lambda (x) (%+ x 10)) (%quote (1 2 3 4 5 6 7)))
'(11 12 13 14 15 16 17)

;;assignment 3 
;;extend interpeter with dolist from common lisp
;;(%dolist (var listexpr res) expr1 ... exprn)

;;example dolist in work
(%define (%sumlist lst)
   (%define sum 0)
    (%dolist (x lst sum) (%set! sum (%+ sum x))))
ok
%==> (%sumlist (%quote (1 2 3)))
6


