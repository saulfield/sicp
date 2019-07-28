;;; #lang racket

;;; run with:
;;; racket -i -f chapter4.rkt

;;; Utility -------------------------------------------------

(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-procedure params body env)
  (list 'procedure params body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-params p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-env p) (cadddr p))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc)
        (list 'primitive (cadr proc)))
       primitive-procedures))

(define (primitive-implementation proc)
  (cadr proc))

;;; Eval ----------------------------------------------------

(define (self-evaluating? expr)
  (cond ((number? expr) true)
        ((string? expr) true)
        ((eq? expr #t) true)
        ((eq? expr #f) true)
        (else false)))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp)
  (cadr exp))

(define (eval-if exp env)
  (if (true? (eval-expr (if-predicate exp) env))
      (eval-expr (if-consequent exp) env)
      (eval-expr (if-alternative exp) env)))

(define (eval-expr exp env)
  (cond ((self-evaluating? exp) exp)
        ;;; ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ;;; ((assignment? exp) (eval-assignment exp env))
        ;;; ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ;;; ((lambda? exp)
        ;;;  (make-procedure 
        ;;;   (lambda-parameters exp)
        ;;;   (lambda-body exp)
        ;;;   env))
        ;;; ((begin? exp)
        ;;;  (eval-sequence 
        ;;;   (begin-actions exp) 
        ;;;   env))
        ;;; ((cond? exp) (eval-expr (cond->if exp) env))
        ;;; ((application? exp)
        ;;;  (apply-proc (eval-expr (operator exp) env)
        ;;;              (list-of-values (operands exp)
        ;;;                              env)))
        (else
         (error "Unknown expression type: EVAL" exp))))

;;; Apply ---------------------------------------------------

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))

(define (apply-proc procedure args)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure args))
        ;;; ((compound-procedure? procedure)
        ;;;  (eval-sequence
        ;;;    (procedure-body procedure)
        ;;;    (extend-environment
        ;;;      (procedure-parameters procedure)
        ;;;      args
        ;;;      (procedure-environment procedure))))
        (else
         (error "Unknown procedure type: APPLY"
                procedure))))

;;; Tests ---------------------------------------------------

(define (test-eval exp expected)
  (define result (eval-expr exp null))
  (if (equal? result expected)
      (printf "Passed: ~s~n" exp)
      (begin 
        (printf "Failed: ~s~n" exp)
        (printf "  Expected: ~s~n" expected)
        (printf "  Actual:   ~s~n" result))))

(define (test-apply proc args expected)
  (define result (apply-proc proc args))
  (if (equal? result expected)
      (printf "Passed: (apply-proc ~s ~s)~n" proc args)
      (begin
        (printf "Failed: (apply-proc ~s ~s)~n" proc args)
        (printf "  Expected: ~s~n" expected)
        (printf "  Actual:   ~s~n" result))))

(printf "Eval Tests -----------------------------------~n")
(test-eval '1 1)
(test-eval '"abc" "abc")
(test-eval '(if #t "true" "false") "true")
(test-eval '(if #f "true" "false") "false")
(test-eval '(if 1 "true" "false") "true")

(printf "~nApply Tests ----------------------------------~n")
(define cons-proc (caddr (primitive-procedure-objects)))
(test-apply cons-proc '(1 2) (cons 1 2))