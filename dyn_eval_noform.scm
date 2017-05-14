;----------------------------------------------------------;
;                       dyn_eval_noform.scm                ;
;----------------------------------------------------------;
; Author : Yoann                                           ;
; Created : Sun May 13 2014                                ;
; Last modification : Sun May 14 21:19:00 2017             ;
; Évaluateur Dynamique                                     ;
;----------------------------------------------------------;

(module dyn-eval-noform)

(define the-false-value (cons "false" "boolean"))
(define empty 813)
(define env.init '())

(define (atom? x)
   (not (pair? x)))

(define (eprogn exps env)
   (if (pair? exps)
       (if (pair? (cdr exps))
	   (let ((result (evaluate (car exps) env)))
	     (let ((value (car result))
		   (env (cdr result)))
	       (eprogn (cdr exps) env)))
	 (evaluate (car exps) env))
     (cons empty env)))

(define (extend env variable value)
  (cons (cons variable value) env))

(define (lookup id env)
   (if (pair? env)
       (if (eq? (caar env) id)
	    (cdar env)
	   (lookup id (cdr env)))
       (error "lookup" "no such binding" id)))

(define (update id env v)
  (extend env id v))

(define (evaluate exp env)
  (if (atom? exp)
      (cond ((symbol? exp) (cons (lookup exp env) env))
	    ((or (number? exp) (string?	exp) (char? exp) (boolean? exp) (vector? exp))
	     (cons exp env))
	    (else (error "evaluate" "cannot evaluate" exp)))
    (case (car exp)
      ((quote) (cons (cadr exp) env))
      ((if) (let ((result (evaluate (cadr exp) env)))
	      (let ((value (car result))
		    (cond-env (cdr result)))
		(if (not (equal? value the-false-value))
		    (evaluate (caddr exp) env)
		  (evaluate (cadddr exp) env)))))
      ((begin) (eprogn (cdr exp) env))
      ((set) (let ((result (evaluate (caddr exp) env)))
	       (let ((value (car result))
		     (env (cdr result)))
		 (cons empty (update (cadr exp) env value)))))
      ((let) (let ((result (evaluate (cadadr exp) env)))
	       (let ((value (car result))
		     (let-env (cdr result)))
		 (eprogn (cddr exp) (extend env (caadr exp) value)))))
      ((lambda) (cons (make-closure (caadr exp) (cddr exp) env) env))
      ((function) (cons (make-function (caadr exp) (cddr exp) env) env))
      ((+) (let ((result (evaluate (cadr exp) env)))
	     (let ((left-value (car result))
		   (left-env (cdr result)))
	       (let ((result (evaluate (caddr exp) env)))
		 (let ((right-value (car result))
		       (right-env (cdr result)))
		   (cons (+ left-value right-value)
			 env))))))
      (else 
       (let ((result (evaluate (car exp) env)))
	 (let ((f (car result))
	       (env (cdr result)))
	   (let ((result (evaluate (cadr exp) env)))
	     (let ((value (car result))
		   (env (cdr result)))
	       (invoke f value env)))))))))

(define (invoke fn arg dyn-env)
   (if (procedure? fn)
       (fn arg dyn-env)
       (error "invoke" "not a procedure" fn)))

(define (make-closure variable body env)
   (lambda (value dyn-env)
      (eprogn body (extend env variable value))))

(define (make-function variable body env)
   (lambda (value dyn-env)
      (eprogn body (extend dyn-env variable value))))

(let loop ((exprs '((1 (let (a 1) a))
		    ;;((3 4) (let (a 2) (let (a 3) (let (b 4) (list a b)))))
		    (12 (let (f (lambda (x) x)) (f 12)))
		    (12 (let (f (function (x) x)) (f 12)))
		    (3 (let (x 0)
			  (let (fl (lambda (a) (+ x a)))
			    (let (ff (function (a) (+ x a)))
			      (set x 1)
			      (+ (fl 1) (ff 1))))))
		    (3 (let (x 0)
			  (let (fl (lambda (a) (+ x a)))
			    (let (ff (function (a) (+ x a)))
			      (set x 1)
			      (+ (ff 1) (fl 1))))))
		    )))
     (if (pair? exprs)
	 (let ((expected-result (caar exprs))
	       (expression (cadar exprs)))
	  (let ((result (evaluate expression env.init)))
	    (let ((value (car result))
		  (env (cdr result)))
	      (if (equal? value expected-result)
		  (begin
		   (print expression)
		   (print "evaluates to")
		   (print value))
		(error "assert false" result (car (car exprs))))))
	  (loop (cdr exprs)))))
