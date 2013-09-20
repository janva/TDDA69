;;initial test of interpeter
 (%+ (%* 6 7) 8)
;;output 
50

(%cons (%quote (a b c)) (%quote (d e f)))
;;out
((a b c) d e f)

(%define (%factorial x)
	 (%if (%< x 1)
	      a
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
;;om jag forstår det rätt är res värdet som returneras, var variable
;; som används för att lagra ett element i taget ur listan, och listan
;; är listan av element expr1 ... exprn är utrryck som appliceras på
;; var och eventuel res 

;;Detta är en special form

;;föjande beskrivning gavs
;;
;; Semantiken för %dolist ska vara följande:
;; Styrvariabeln var ska definieras som en lokal variabel i en ny omgivning.
;; Uttryck listexpr evalueras sedan och ska resultera i en lista. För 
;; varje värde i denna lista ska
;;följande utföras:
;;o Sätt variablen var till det aktuella värdet.
;;o Evaluera uttrycken expr1 till exprn.
;; Till sist evalueras uttrycket res vars värde returneras som värde 
;; för hela %dolist-uttrycket. Efteråt ska bindningen till styrvariabeln 
;;var inte finnas kvar.
;;
;;(%dolist (var listexpr res) expr1 ... exprn)

;;assignment 4
Utöka %Scheme med de konstruktioner som behövs för att hantera strömmar på samma sätt
som i laboration 2, d.v.s. vi önskar %delay, %force, %cons-stream, %stream-car,
 %stream-cdr, %the-empty-stream och %stream-null?. Innan ni skriver kod, fundera över vilka av dessa konstruktioner som måste implementeras som special forms och vilka som kan implementeras direkt i %Scheme. I kursbiblioteket i filen streams.%ss finns alla de övriga strömhanteringsfunktionerna, motsvarande de i streams.ss, men anpassade till %Scheme.

;;special form
%delay, 
;; just application of its arguement
%force, 
;;special form
%cons-stream,
%stream-cdr, 
;;syntactic suger for (force (cdr stream)) hmmm could be implemented anywhere


;;implemented;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;as variable global enviroment
%the-empty-stream 
;;equivalen to null? 
%stream-null?

;;equivalent to car 
%stream-car,

;;test
(%define int-from-3 (integers-starting-from 3))


(define (load-and-init) 
  (load "meta_eval.ss")
  (init-%scheme))

