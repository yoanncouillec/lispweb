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

(define (eprogn exps lex-env dyn-env)
   (if (pair? exps)
       (if (pair? (cdr exps))
	   (begin (d.evaluate (car exps) lex-env dyn-env)
		  (eprogn (cdr exps) lex-env dyn-env))
	   (d.evaluate (car exps) lex-env dyn-env))
       empty-begin))

(define (evlis exps lex-env dyn-env)
   (if (pair? exps)
       (cons (d.evaluate (car exps) lex-env dyn-env)
	  (evlis (cdr exps) lex-env dyn-env))
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

(define (d.evaluate exp lex-env dyn-env)
  (if (atom? exp)
      (cond ((symbol? exp) (lookup exp lex-env))
	    ((or (number? exp) (string? exp) (char? exp) (boolean? exp) (vector? exp))
	     exp)
	    (else (error "evaluate" "cannot evaluate" exp)))
    (case (car exp)
      ((quote) (cadr exp))
      ((if) (if (not (eq? (d.evaluate (cadr exp) lex-env dyn-env) the-false-value))
		(d.evaluate (caddr exp) lex-env dyn-env)
	      (d.evaluate (cadddr exp) lex-env dyn-env)))
      ((begin) (eprogn (cdr exp) lex-env dyn-env))
      ((dynamic) (lookup (cadr exp) dyn-env))
      ((set!) (update! (cadr exp) lex-env (d.evaluate (caddr exp) lex-env dyn-env)))
      ((set-dynamic!) (update! (cadr exp) dyn-env (d.evaluate (caddr exp) lex-env dyn-env)))
      ((let) (eprogn (cddr exp) (extend lex-env (list (caadr exp)) (list (d.evaluate (cadadr exp) lex-env dyn-env))) dyn-env))
      ((let-dynamic) (eprogn (cddr exp) lex-env (extend dyn-env (list (caadr exp)) (list (cadadr exp)))))
      ((list) (apply list (evlis (cdr exp) lex-env dyn-env)))
      ((closure) (d.make-closure (cadr exp) (cddr exp) lex-env dyn-env))
      ((call) (d.invoke (d.evaluate (cadr exp) lex-env dyn-env)
			(evlis (cddr exp) lex-env dyn-env)
			dyn-env))
      (else (error "d.evaluate" "unknown form" (car exp))))))

(define (d.invoke fn args dyn-env)
   (if (procedure? fn)
       (fn args dyn-env)
       (error "d.invoke" "not a procedure" fn)))

;; lexical
(define (d.make-closure variables body lex-env unused-dyn-env)
   (lambda (vals dyn-env)
      (eprogn body (extend lex-env variables vals) dyn-env)))

;; dynamic
;;(define (d.make-function variables body lex-env unused-dyn-env)
;;  (lambda (vals dyn-env)
;;    (eprogn body lex-env (extend dyn-env variables vals))))

;;(define toto '((lambda (a) ((lambda (f) (begin (set! a 10) (f))) (lambda () a))) 1))
;; (define tata '((lambda (a) 
;; 		 (((lambda (a) 
;; 		     (lambda (b) 
;; 		       (list a b))) 
;; 		   2)
;; 		  3)
;; 		 ) 
;; 	       1)
;;   )
;;(define titi '((function (a) (((function (a) (function (b) (list a b))) 2) 3)) 1))
;;(print (d.evaluate tata env.init env.init))

(let ((v1 (d.evaluate '(let (a 1) a) env.init env.init))
      (v2 (d.evaluate '(let (a 2) (let (a 3) (let (b 4) (list a b)))) env.init env.init))
      (v3 (d.evaluate '(let-dynamic (a 5) (let (a 6) (let (b 7) (list (dynamic a) b)))) env.init env.init))
      (v4 (d.evaluate '(let-dynamic (a 8) (let (f (closure (x) (set-dynamic! a x))) (begin (call f 9) (dynamic a)))) env.init env.init))
      )
  [assert (v1) (equal? 1 v1)]
  (print v1)
  [assert (v2) (equal? (list 3 4) v2)]
  (print v2)
  [assert (v3) (equal? (list 5 7) v3)]
  (print v3)
  [assert (v4) (equal? 9 v4)]
  (print v4)
  )
