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
