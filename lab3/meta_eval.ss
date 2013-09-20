
;;; --------------------------------------------------------------------------
;;;  meta_eval.ss
;;;  Evaluator for the %Scheme language
;;;
;;;  Original code from "Structure and Interpretation of Computer Programs"
;;;  by Abelson & Sussman. Adapted for use in the course "Data and Program
;;;  Structures" (TDDA69).
;;; --------------------------------------------------------------------------

;; (load "TDDA69/Lab/abssyntax.ss")
;; (load "TDDA69/Lab/environment.ss")

;;(load "/home/TDDA69/Lab/r6rs/abssyntax.ss")
;;(load "/home/TDDA69/Lab/r6rs/environment.ss")
;;(%cons (%quote (a b c)) (%quote (d e f)))

;;(load "abssyntax.ss") 
;;(load "environment.ss")

(load "/home/janva/programmering/kurser/tdda69.git/lab3/abssyntax.ss") 
(load "/home/janva/programmering/kurser/tdda69.git/lab3/environment.ss")

;;; Core of the evaluat or
;;; ---------------------
(define (eval-%scheme exp env)
  (cond ((self-evaluating? exp) exp)
	;; hmmm which ones do i realy need to put here 
	((delay? exp)  (eval-delay exp env) )
	((force? exp)  (eval-force exp env) )
	((cons-stream? exp)  (eval-cons-stream exp env) )
	((stream-cdr? exp)  (eval-stream-cdr exp env) )
	
	((dolist? exp)  (eval-dolist exp env) )
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval-%scheme (cond->if exp) env))
        ((application? exp)
	 (apply-%scheme (eval-%scheme (operator exp) env)
			(list-of-values (operands exp) env)))
        (else
         (error 'eval-%scheme "Unknown expression type: ~s" exp))))

(define (apply-%scheme procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
	  (procedure-body procedure)
	  (extend-environment
	   (procedure-parameters procedure)
	   arguments
	   (procedure-environment procedure))))
        (else
         (error 'apply-%scheme "Unknown procedure type: ~s" procedure))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;streams 
;;
(define DEBUG #f)

(define (debug-me expr name)
  ;; TODO evolve this
  (newline)
  (display "-----------")
  (display name)
  (display " ------------")
  (newline)
  (display expr))


(define (eval-stream-cdr exp env)
  (when DEBUG
    (debug-me  (cadr exp) "eval-stream-cdr/////////")
    (newline))
  (eval-%scheme (list '%force (list '%cdr
		      (cadr exp))) env))

;;check wheter this works
(define (eval-cons-stream exp env)
  (when DEBUG
    (debug-me exp "eval-cons-stream")
    (newline)  
    (debug-me (eval-%scheme 
	       (stream-car-expr exp) env) "eval-cons-stream.car part")
    (newline)
    (debug-me (eval-%scheme  
	       (make-delayed-expression exp) env)  "eval-cons-stream.cdr part")
    ;;(newline)
    ;;(display (stream-car-expr exp))
    ;;(newline)
    ;;(display (stream-cdr-expr exp))
    (newline))
  ;;(display (list '%cons (stream-car-expr exp) 
					;,(list '%delay (stream-cdr-expr exp)))))
  ;;simplify this
  (cons
   (eval-%scheme 
    (stream-car-expr exp) env)
   (eval-%scheme  
    (make-delayed-expression exp) env)))
;; (eval-%scheme  (list '%cons (stream-car-expr exp) 
;;		       (list '%delay (stream-cdr-expr exp))) env))

;;TODO into abssyntax

(define (make-delayed-expression exp)
  (when DEBUG
    (debug-me  (list '%delay 
		     (stream-cdr-expr exp)
		     (cadr exp))
	       "make-delayed-expression"))
  (list '%delay 
	(stream-cdr-expr exp)))
	;;(cadr exp)))
  

(define (eval-delay exp env) 
  (when DEBUG
    (newline)
    (debug-me  exp "eval-delay-(exp)")
    (newline)
    (debug-me  (make-lambda '() (cdr exp)) "eval-delay"))
  (eval-%scheme
   (make-lambda '() (cdr exp)) env))

(define (eval-force exp env)
    (when DEBUG
      (debug-me  exp  "eval-force (delayed-object)")
    (newline))
    ;; apply procedure since procedurecer
    ;;(display "foooorceeee it ")
    ;; cons-stream skall skapa punkterat par
    (eval-%scheme  (delayed-object exp)  env))

(define (delayed-object exp)
  (cdr exp))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;dolist
(define (eval-dolist-helper var lst expr-list env res )
  (cond ((null? (cdr lst)) 
	 (set-variable-value! var (car lst) env)
	 (eval-sequence expr-list env)
	 (eval-%scheme res env))
	(else
	 (set-variable-value! var (car lst) env)
	 (eval-sequence expr-list env)
	 (eval-dolist-helper var (cdr lst) expr-list env res))) 
  (eval-%scheme res env))
	 

(define (eval-dolist exps env)
  (eval-dolist-helper (dolist-temp-variable exps) 
		      ;;evaluated given the env before extension?
		      (eval-%scheme (dolist-element-list exps)env)
		      (dolist-expressions-block exps)
		      (extend-environment 
		       (list(dolist-temp-variable  exps))
		       ;;(dolist-res-var-name exps))
		       '(0 )
		       env)
		      (dolist-res-var-name exps)))


  


(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval-%scheme (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (eval-%scheme (if-predicate exp) env))
      (eval-%scheme (if-consequent exp) env)
      (eval-%scheme (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval-%scheme (first-exp exps) env))
        (else (eval-%scheme (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval-%scheme (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval-%scheme (definition-value exp) env)
                    env)
  'ok)

;;; Representing procedure objects
;;; ------------------------------

(define (make-procedure parameters body env)
  (list '$procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p '$procedure))

(define (procedure-parameters p) (cadr p))

(define (procedure-body p) (caddr p))

(define (procedure-environment p) (cadddr p))

(define (primitive-procedure? proc)
  (tagged-list? proc '$primitive))

(define (primitive-implementation proc) (cadr proc))

(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))

;;; User interface
;;; --------------

(define (driver-loop)
  (newline)
  (display "%==> ")
  (do ((%exp (read)(read)))
      ((memq %exp '(exit stop logout hej-d�)) 
       (display "Have a nice day!") (newline))
      (user-print (eval-%scheme %exp the-global-environment))
      (newline)
      (display "%==> ")))

(define (user-print object)
  (define (circular-print object)
    (cond ((or (not (list? object)) (eq? object '())) object)
	  ((compound-procedure? object)  
	   (list '<$procedure
		 (procedure-parameters object)
		 (procedure-body object)
		 '<procedure-environment>>))
	  ((primitive-procedure? object)
	   (list '<$primitive>))
	  (else (cons (circular-print (car object))
		      (circular-print (cdr object))))))
  (display (circular-print object)))

;; (init-%scheme) initializes the global environment, and calls driver-loop.
;; To start %Scheme without initializing, call (go-%scheme).

(define (init-%scheme)
  (set! the-global-environment (setup-environment))
  (driver-loop))

(define (go-%scheme)
  (driver-loop))

;; Useful abbreviations

(define init init-%scheme)
(define go go-%scheme)

;; (remote-eval exp) makes it possible to evaluate %scheme-expressions from
;; Chez Scheme. The main use of this is to make definitions in the global
;; environment, without entering the interactive driver-loop.

(define (remote-eval %-exp)
  (user-print (eval-%scheme %-exp the-global-environment))
  (newline))

;;; Examples
;;; --------

;; Here are some examples that you may want to try out:

;; (remote-eval '(%define (fact n) (%if (%= 1 n) 1 (%* n (fact (%- n 1))))))
;; (remote-eval '(fact 10))


;;; --------------------------------------------------------------------------
(display "Loaded meta_eval.ss")
(newline)

