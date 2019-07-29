;;; Utility -------------------------------------------------

(define (true? x) (not (eq? x #f)))
(define (false? x) (eq? x #f))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate   exp) (cadr  exp))
(define (if-consequent  exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-procedure params body env)
  (list 'procedure params body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-params p) (cadr   p))
(define (procedure-body   p) (caddr  p))
(define (procedure-env    p) (cadddr p))

(define primitive-procedures
  (list (list 'car   car)
        (list 'cdr   cdr)
        (list 'cons  cons)
        (list 'null? null?)))
(define (primitive-procedure-names)
  (map car primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc)
        (list 'primitive (cadr proc)))
       primitive-procedures))
(define (primitive-implementation proc)
  (cadr proc))

;;; Environment ---------------------------------------------

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied"  vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars)
                        (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars)
                        (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) 
                        (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

(define (setup-environment)
  (let ((initial-env
         (extend-environment
          (primitive-procedure-names)
          (primitive-procedure-objects)
          the-empty-environment)))
    (define-variable! 'true  #t initial-env)
    (define-variable! 'false #f initial-env)
    initial-env))

(define the-global-environment (setup-environment))

;;; Eval ----------------------------------------------------

(define (self-evaluating? expr)
  (cond ((number? expr) #t)
        ((string? expr) #t)
        ((eq? expr #t)  #t)
        ((eq? expr #f)  #t)
        (else #f)))

(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

(define (variable? exp) (symbol? exp))

(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))
(define (eval-assignment exp env)
  (set-variable-value! 
   (assignment-variable exp)
   (eval (assignment-value exp) env)
   env)
  'ok)

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda
       (cdadr exp)   ; formal parameters
       (cddr exp)))) ; body
(define (eval-definition exp env)
  (define-variable! 
    (definition-variable exp)
    (eval-expr (definition-value exp) env)
    env)
  'ok)

(define (eval-if exp env)
  (if (true? (eval-expr (if-predicate exp) env))
      (eval-expr (if-consequent  exp) env)
      (eval-expr (if-alternative exp) env)))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (list-of-values exps env)
  (if (null? exps)
      '()
      (cons (eval-expr (car exps) env)
            (list-of-values (cdr exps) env))))

(define (eval-expr exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable?   exp) (lookup-variable-value exp env))
        ((quoted?     exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure
          (lambda-parameters exp)
          (lambda-body exp)
          env))
        ;;; ((begin? exp)
        ;;;  (eval-sequence 
        ;;;   (begin-actions exp) 
        ;;;   env))
        ;;; ((cond? exp) (eval-expr (cond->if exp) env))
        ((application? exp)
         (apply-proc (eval-expr (operator exp) env)
                     (list-of-values (operands exp) env)))
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
         (error "Unknown procedure type: APPLY" procedure))))

;;; Tests ---------------------------------------------------

(define (test-eval-env exp env expected)
  (define result (eval-expr exp env))
  (if (equal? result expected)
      (printf "Passed: (eval-expr ~s)~n" exp)
      (begin 
        (printf "Failed: ~s~n" exp)
        (printf "  Expected: ~s~n" expected)
        (printf "  Actual:   ~s~n" result))))

(define (test-eval exp expected)
  (test-eval-env exp (setup-environment) expected))

(printf "Expression tests -----------------------------~n")
(test-eval '1 1)
(test-eval '(quote a) 'a)
(test-eval '"abc" "abc")
(test-eval '(if 5  "true" "false") "true")
(test-eval '(if #t "true" "false") "true")
(test-eval '(if #f "true" "false") "false")
(test-eval '(if true  "true" "false") "true")
(test-eval '(if false "true" "false") "false")
(test-eval '(cons 1 2) (cons 1 2))
(test-eval '(define x 1) 'ok)
(test-eval '(define x (cons 1 2)) 'ok)
(test-eval '(define f (lambda () 'result)) 'ok)
(test-eval '(define (f) 'result) 'ok)

(printf "~nEnvironment tests ----------------------------~n")
(define test-env (setup-environment))
(test-eval-env '(define x 1) test-env 'ok)
(test-eval-env '(define y 2) test-env 'ok)
(test-eval-env '(define z (cons x y)) test-env 'ok)
(test-eval-env 'x test-env 1)
(test-eval-env 'y test-env 2)
(test-eval-env 'z test-env (cons 1 2))
