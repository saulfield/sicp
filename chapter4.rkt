;;; #lang racket

;;; Utility

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

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (eval-if exp env)
  (if (true? (eval-expr (if-predicate exp) env))
      (eval-expr (if-consequent exp) env)
      (eval-expr (if-alternative exp) env)))

;;; Evaluator

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
        ;;; ((cond? exp) (eval (cond->if exp) env))
        ;;; ((application? exp)
        ;;;  (apply (eval (operator exp) env)
        ;;;         (list-of-values 
        ;;;          (operands exp) 
        ;;;          env)))
        (else
         (error "Unknown expression 
                 type: EVAL" exp))))

;;; Tests

(define (test exp expected)
  (define result (eval-expr exp null))
  (if (eq? result expected)
      (displayln "Passed")
      (begin 
        (printf "Failed: ~s~n" exp)
        (printf "  Expected: ~s~n" expected)
        (printf "  Actual: ~s~n" result))))

(test '1 1)
(test '"abc" "abc")
(test '(if #t "true" "false") "true")
(test '(if #f "true" "false") "false")
(test '(if 1 "true" "false") "true")