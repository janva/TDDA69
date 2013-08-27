;;1a)
;;code from sicp 3.5.2
(define ones (cons-stream 1 ones))
(define integers
  (cons-stream 1 (add-streams ones integers)))
;;test 
(print-stream integers 9)
;;outputs
[ 1 2 3 4 5 6 7 8 9 ... ]


;;Definiera sedan en ny ström med alla heltal som inte är delbara 
;;med 2, 3 eller 5. Använd funktionen stream-filter för detta.

;;dependency stream.ss divisible? , stream-filter
(define odelbara 
  (stream-filter2 not-div-2-3-5
		 integers))
  
(define (not-div-2-3-5 x)
  (not ( or (divisible? x 2)
	    (divisible? x 3)
	    (divisible? x 5))))
 
;;test 
(print-stream odelbara 20)
;;output
[ 1 7 11 13 17 19 23 29 31 37 41 43 47 49 53 59 61 67 71 73 ... ]