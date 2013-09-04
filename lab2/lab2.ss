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

;;predicate integer not divisible   2 3 5
(define (not-div-2-3-5 x)
  (not ( or (divisible? x 2)
	    (divisible? x 3)
	    (divisible? x 5))))

;;test 
(print-stream odelbara 20)
;;output
[ 1 7 11 13 17 19 23 29 31 37 41 43 47 49 53 59 61 67 71 73 ... ]

;;TODO procedure which returns stream instead 
(define undivisble-3
  (stream-filter2 not-div-3
		  integers))

(define undivisble-7
  (stream-filter2 not-div-7
		  integers))

;;predicate integer not divisble by 3
(define (not-div-3 x)
  (not(divisible? x 3)))

;;predicate integer not divisble by 7
(define (not-div-7 x)
  (not(divisible? x 7)))
 

(define interleaved-3-7 
  (interleave2 undivisble-3 undivisble-7))

;;test
(print-stream interleaved-3-7 20)
;;output
[ 1 1 2 2 4 3 5 4 7 5 8 6 10 8 11 9 13 10 14 11 ... ]

;;assignment 3 
;;a)

(define 2-factored 
  (scale-stream 2 integers))
(define 3-factored 
  (scale-stream 3 integers))
(define 5-factored 
  (scale-stream 5 integers))

(define 2-3-5-faktored
  (merge 2-factored
	 (merge 3-factored 5-factored)))

(define merged-factored
  (cons-stream 1 2-3-5-faktored))

;;test 
(print-stream merged-factored 20)
;; output 
[ 1 2 3 4 5 6 8 9 10 12 14 15 16 18 20 21 22 24 25 26 ... ]

;; 6 numbers following 406
 ;;408 410 411 412 414 415

;;b) Annother way to create the stream would be to put the integers through a 
;; the stream-filter. This would be less effective and i imagine, not as
;; transparent design. That is the design above is probably easier to
;; write and understand.

;;assignement 4 TODO TODO TODO later
;;to get mutable pair in racket
(require racket/mpair )

;; new primitives
;; under construction i'm not sure i've understod the assignment
(define-syntax 
   cons-mstream 
   (syntax-rules ()
     ((cons-mstream head tail)
      (mcons head (delay tail)))))

;;hmmm is this what they want
(define (stream-mcdr! stream)
 (set-mcdr! stream (force (mcdr stream))))

(define a
  (cons-mstream
   1
   (cons-mstream
    2
    (cons-mstream
     3
     the-empty-mstream))))

(define b
  (mcons
   1
   (mcons
    2
    (mcons
     3
     null))))
(mcdr (mcdr  b))
