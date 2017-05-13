;----------------------------------------------------------;
;                       dyn_eval.scm                       ;
;----------------------------------------------------------;
; Author : Yoann                                           ;
; Created : Sun Oct 19 09:29:30 2014                       ;
; Last modification : Sun Oct 19 09:29:30 2014             ;
; Évaluateur Dynamique                                     ;
;----------------------------------------------------------;

(module dyn-eval)

(define the-false-value (cons "false" "boolean"))
(define empty-begin 813)
(define env.init '())

(define (atom? x)
   (not (pair? x)))

(define (eprogn exps env)
   (if (pair? exps)
       (if (pair? (cdr exps))
	   (begin (d.evaluate (car exps) env)
		  (eprogn (cdr exps) env))
	   (d.evaluate (car exps) env))
       empty-begin))

(define (evlis exps env)
   (if (pair? exps)
       (cons (d.evaluate (car exps) env)
	  (evlis (cdr exps) env))
       '()))

(define (lookup id env)
   (if (pair? env)
       (if (eq? (caar env) id)
	   (cdar env)
	   (lookup id (cdr env)))
       (error "lookup" "no such binding" id)))

(define (update! id env v)
   (if (pair? env)
       (if (eq? (caar env) id)
	   (set-cdr! (car env) v)
	   (update! id (cdr env) v))
       (error "lookup" "no such binding" id)))

(define (extend env variables values)
   (cond ((pair? variables)
	  (if (pair? values)
	      (cons (cons (car variables)
		       (car values))
		 (extend env (cdr variables)
		    (cdr values)))
	      (error "extend" "too less values" values)))
	 ((null? variables)
	  (if (null? values)
	      env
	      (error "extend" "too much values" values)))
	 ((symbol? variables)
	  (cons (cons variables values) env))))

(define (d.evaluate exp env)
   (if (atom? exp)
       (cond ((symbol? exp) (lookup exp env))
	     ((or (number? exp) (string? exp) (char? exp) (boolean? exp) (vector? exp))
	      exp)
	     (else (error "evaluate" "cannot evaluate" exp)))
       (case (car exp)
	  ((quote) (cadr exp))
	  ((if) (if (not (eq? (d.evaluate (cadr exp) env) the-false-value))
		    (d.evaluate (caddr exp) env)
		    (d.evaluate (cadddr exp) env)))
	  ((begin) (eprogn (cdr exp) env))
	  ((set!) (update! (cadr exp) env (d.evaluate (caddr exp) env)))
	  ((list) (apply list (evlis (cdr exp) env)))
	  ((function) (d.make-closure (cadr exp) (cddr exp) env))
	  ((lambda) (d.make-function (cadr exp) (cddr exp)))
	  (else (d.invoke (d.evaluate (car exp) env)
		   (evlis (cdr exp) env)
		   env)))))

(define (d.invoke fn args d.env)
   (if (procedure? fn)
       (fn args d.env)
       (error "d.invoke" "not a procedure" fn)))

(define (d.make-function variables body)
   (lambda (vals d.env)
      (eprogn body (extend d.env variables vals))))

(define (d.make-closure variables body env)
   (lambda (vals d.env)
      (eprogn body (extend env variables vals))))

(define toto '((lambda (a) ((lambda (f) (begin (set! a 10) (f))) (lambda () a))) 1))
(define tata '((lambda (a) (((lambda (a) (lambda (b) (list a b))) 2) 3)) 1))
(define titi '((function (a) (((function (a) (function (b) (list a b))) 2) 3)) 1))
(print (d.evaluate tata env.init))
