
;;; --------------------------------------------------------------------------
;;;  streams.ss
;;;  Stream functions
;;;
;;;  Original code from "Structure and Interpretation of Computer Programs"
;;;  by Abelson & Sussman. Adapted for use in the course "Data and Program
;;;  Structures" (TDDA69).
;;; --------------------------------------------------------------------------

;;; --------------------------------------------------------------------------
;;;  Stream primitives
;;; --------------------------------------------------------------------------

;; The cons-stream construct
;; -------------------------

;; Note that there are different versions for different Scheme
;; implementations! Uncomment the syntax that you want to use.

;; THIS SYNTAX IS VALID FOR CHEZ SCHEME

;; (extend-syntax 
;;  (cons-stream) 
;; ((cons-stream head tail)
;;  (cons head (delay tail))))
;;
;; Equivalent version, perhaps easier to visualize:
;;
;; (extend-syntax 
;;  (cons-stream) 
;;  ((cons-stream head tail)
;;   (cons head (lambda () tail))))

;; THIS SYNTAX IS VALID FOR DR SCHEME (MZ SCHEME)

;; (define-macro cons-stream
;;  (lambda (a b)
;;    `(cons ,a (delay ,b))))


;; this syntax worked for my scheme implementation 
;; THIS SYNTAX IS VALID FOR SOME OTHER KIND OF SCHEME IMPLEMENTATION

 (define-syntax 
   cons-stream 
   (syntax-rules ()
     ((cons-stream head tail)
      (cons head (delay tail)))))
;;
;; Other stream primitive
;; -----------------------

(define stream-null? null?)

(define the-empty-stream '())

(define stream-car car)

(define (stream-cdr stream)
  (force (cdr stream)))

;; Equivalent version, perhaps easier to visualize:
;;
;; (define (stream-cdr stream)
;;   ((cdr stream)))

;;; --------------------------------------------------------------------------
;;;  Stream utility functions
;;; --------------------------------------------------------------------------

(define (stream-ref stream n)
  (if (= n 0)
      (stream-car stream)
      (stream-ref (stream-cdr stream) (- n 1))))
;;only worked after renaming to stream-filter2
(define (stream-filter2 pred stream)
  (cond ((stream-null? stream)
         the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter2 pred (stream-cdr stream))))
        (else (stream-filter2 pred (stream-cdr stream)))))

(define (stream-map proc . streams)
  (if (stream-null? (car streams))
      the-empty-stream
      (cons-stream (apply proc (map stream-car streams))
		   (apply stream-map (cons proc (map stream-cdr streams))))))

(define (stream-for-each proc stream)
  (if (stream-null? stream)
      'done
      (begin (proc (stream-car stream))
	     (stream-for-each proc (stream-cdr stream)))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low (stream-enumerate-interval (+ low 1) high))))

;; Streams of numbers
;; ------------------

(define (add-streams s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else (cons-stream (+ (stream-car s1) (stream-car s2))
                           (add-streams (stream-cdr s1) 
					(stream-cdr s2))))))

(define (scale-stream c stream)
  (stream-map (lambda (x) (* x c))
       stream))

;; Combining streams
;; -----------------

;; This version of interleave differs from the book by not 
;; checking for the empty stream.

(define (interleave2 s1 s2)
  (cons-stream (stream-car s1)
               (interleave2 s2 (stream-cdr s1))))

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else (let ((h1 (stream-car s1))
                    (h2 (stream-car s2)))
                (cond ((< h1 h2)
                       (cons-stream h1 (merge (stream-cdr s1) s2)))
                      ((< h2 h1)
                       (cons-stream h2 (merge s1 (stream-cdr s2))))
                      (else (cons-stream h1 (merge (stream-cdr s1)
						   (stream-cdr s2)))))))))

;; Stream I/O
;; ----------
  
(define print-stream
  ;; This obscure definition saves a lot of cons cells during 
  ;; garbage collection!
  (let ()
    (define (iter s n)
      (cond ((stream-null? s) (display "]"))
	    ((zero? n) (display "... ]"))
	    (else (begin (display (stream-car s))
			 (display " ")
			 (iter (stream-cdr s) (- n 1))))))
    (lambda (s n)
      (display "[ ")
      (iter s n)
      (newline))))

;; Auxiliary functions
;; -------------------

(define (divisible? x y)
  (= (remainder x y) 0))

(define (show x)
  (display x) x)

;; The sieve of Eratosthenes (see section 3.5.2 in SICP)
;; -----------------------------------------------------

(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x)
             (not (divisible? x (stream-car stream))))
           (stream-cdr stream)))))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define primes (sieve (integers-starting-from 2)))



(define (user-print object)
  (define (special-object? object)
    (or (compound-procedure? object)
	(primitive-procedure? object)))
  (define (print-special-object object)
    (cond ((compound-procedure? object)
	   (display (list '$procedure 
			  (procedure-parameters object)
			  (procedure-body object)
			  '<procedure-environment>)))
	  ((primitive-procedure? object)
	   (display (list '$primitive '<procedure>)))))
  (define (print-sequence sequence)
    (cond ((null? sequence)
	   (display ")"))
	  ((atom? sequence)
	   (display ". ")
	   (display sequence)
	   (display ")"))
	  ((special-object? sequence)
	   (display ". ")
	   (print-special-object sequence)
	   (display ")"))
	  (else
	   (user-print (car sequence))
	   (when (not (null? (cdr sequence)))
	       (display " "))
	   (print-sequence (cdr sequence)))))
  (cond ((special-object? object)
	 (print-special-object object))
	((pair? object)
	 (display "(")
	 (print-sequence object))
	(else (display object))))
