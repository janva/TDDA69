
;;this procedure will cause infinite loop
(define (p) (p))

(define (test x y)
  (if (= x 0) 0 y))

;;
;; If APPLICIVE order is used this will cause infinite loop.
;; operands and operator are evaluated then operators are applied on operands.
;; Since we need to evaluate the operand (p) which evaluates to (p) .... 
;; 
;;If NORMAL order is used the expression will evaluate to 0. Because operands 
;;are evaluated only when needed. So 
;;(test 0(p)) => (if (= x 0) 0 y)=> (if (= 0 0)0 (p))=> 0 
;; since if statements predicate evaluates to true only consequent (0) is evaluated.
;;
(test 0 (p))


;;Uppgift 2: Special forms (SICP Exercise 1.6, s.25)
;;Varför måste if vara en special form ? Antag att vi definierar en egen
;;if-funktion så här:
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
	(else else-clause)))
;;Förklara vad som händer om man försöker
;;beräkna kvadratrötter med följande funktion:
(define (sqrt-iter guess x)
  (new-if (good-enough? guess x) guess
	  (sqrt-iter (improve guess x) x)))
;;Beskriv vad som händer när man anropar funktionen
;; sqrt-iter. Förklara varför if måste vara en special form. Motivera era svar.
;; Hmm the problem is in this case that since new-if is not special-form else 
;; clause (sqrt-iter (improve guess x) x) will always be evaluated as well.
;; sqrt-iter makes a recursive call which cause an infinite loop  because 
;; that recursive call needs to evaluate sqrt-iter again due to both clauses 
;; are evaluated because new-if isn't special form and so on and so on....

;;Uppgift 3
;;Antag att vi har följande funktion för att beräkna summor:
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b) )))
;;Funktionen ger upphov till en linjärt rekursiv process, vad vi brukar kalla rekursiv process-
;;lösning. Skriv om funktionen med hjälp av nedanstående mall så att en iterativ processlösning
;;genereras.
;;The trick here is of course first to get an idea what the recursive procedure is doing. 
;;Then to store intermediate result in some variable in this case there store in result variable
(define (sum-iter term a next b)
  (define (iter a result)
    (if(> a b)
       result
       (iter (next a) (+ result (term a)))))
  (iter a 0))


;;factors calcs a factor to be multiplied, first is the starting point i.e the first factor next
;; is function that calculates the next input in product series and last is the limit
;; where the calculation ends first implements recursive-process
(define (product  factors first next last )
  (if (> first  last)
     1
     (* (factors first)
	(product factors (next first) next last))))

;;this is the iterative process
(define (product-iter factors first next last)
  (define (iter first-it result)
    (if (> first-it  last)
	result
	(iter (next first-it) 
	      (* result (factors first-it)))))
  (iter first 1))
;;Testing the functions 
(product (lambda (x)x ) 1 (lambda (y) (+ y 1)) 3)

>(product #<procedure> 1 #<procedure> 3)
> (product #<procedure> 2 #<procedure> 3)
> >(product #<procedure> 3 #<procedure> 3)
> > (product #<procedure> 4 #<procedure> 3)
< < 1
< <3
< 6
<6
6
 
(product-iter (lambda (x)x ) 1 (lambda (y) (+ y 1)) 3)

>(product-iter #<procedure> 1 #<procedure> 3)
<6
6

;; 4B)
;;Definiera fakultetsfunktionen med hjälp av endera av dina
;;product-funktioner

;;could had used id from higher order probably better but 
;; getting use to lambda notation
(define (fak n)
  (product (lambda (x) x) 1 (lambda (y)  (+ y 1))  n )
)
;;test
 (fak 5)
>(product #<procedure> 1 #<procedure> 5)
> (product #<procedure> 2 #<procedure> 5)
> >(product #<procedure> 3 #<procedure> 5)
> > (product #<procedure> 4 #<procedure> 5)
> > >(product #<procedure> 5 #<procedure> 5)
> > > (product #<procedure> 6 #<procedure> 5)
< < < 1
< < <5
< < 20
< <60
< 120
<120
120

;;4C) leaving this for later TODO TODO TODO 
(define (my-pi n)


)

;;5a) 
;; Accumalate is to be a generalization of sum prod that is it
;; a generalisation of same kind of rekursiv process in this case.
;; It has the same params a sum and product. On top of thes it also 
;; has addition parms Combiner is procedure which takes two arguments
;; which combines current term is to be combined with allready 
;; accumaleted terms  null value is the initial value of accumalation
(define (accumulate combiner null-value term a next b)
 (if (> a b)
     null-value
      (combiner
       (accumulate combiner null-value term  (next a) next b)
       term)
      ))
;;test of recursive process version 
(define (acc-sum term a next b)
  (accumulate + 0 term a next b))

(acc-sum (lambda (x)x) 1 (lambda (y) (+ y 1)) 3)

;;output
(acc-sum (lambda (x)x ) 1 (lambda (y) (+ y 1)) 3)
6

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a  b)
	result
	(iter (next a) 
	      (combiner result (term a)))))
  (iter a null-value))

(define (acc-sum-iter term a next b)
  (accumulate-iter * 1 term a next b))

;;test
(acc-sum-iter (lambda (x)x) 1 (lambda (y) (+ y 1)) 3)
6
;;TODO TODO TODO
;; 5C later when order is importent? all tail recursive can be 
;; transformed to iterativ? tree recursive imposible  with linear iterativ process?
(define (id x) x)
;;example of procedures that differ 
(define (acc-diff term a next b)
  (accumulate * 1 term a next b))
fun 
(acc-diff id 1 (lambda (x) (+ x 1)) 5)
;; could this fib procedure be performed by accumalate functions
(define (fib n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	(else (+ (fib (- n 1))
		 (fib (- n 2))))))


;;6 same as acumulate but additional param filter -a procedure which
;; takes one param  predikat over variable that determines wheter or not
;; it is to be accumalated filter is to be applied on indexes  

;; över en variabel som anger vilka termer  som ska kombineras
(define (filter-accumulate filter combiner null-value term a next b)
 (if (> a b)
     null-value
     (if (filter a);; maybe use cond instead
     (combiner (term a)
	       (filter-accumulate filter combiner null-value term  (next a) next b))
     (filter-accumulate filter combiner null-value term  (next a) next b))))
;;6b
;;depence on primitives in higher order higher-order.ss
(define (sum-prime-square a b)
  (filter-accumulate prime? + 0 square a (addx 1) b)
  )
;;test
(sum-prime-square 5  10)
74

;;6C

(define (mult-relative-primes n)
  (define (filter a)
   ;; (display a)
    ;;(display n)
    (if( = (gcd a n) 1)#t #f))
  (filter-accumulate filter * 1 id 1 (addx 1) n)
)
;;test
(mult-relative-primes 4)
3
(mult-relative-primes 5)
24

;;7a for some reason this gives correct result a bit backwards
;; try to understund why
(define (repeated fun n)  
  (if (zero? n )
      id
      (lambda (y)
	((repeated fun (- n 1))
	 (fun y)))))  

;;alternative implementation ?
(define (repeated-2 fun n)  
  (if (zero? n )
      id
      (my-compose (repeated-2 fun (- n 1) )
	       fun)))

;;7B
(define (my-compose f g)
  (lambda (x)
    (f (g x))))


(define (new-repeated fun n)
     (accumulate my-compose id fun 1 (addx 1) n))
  


 
