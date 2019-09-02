;; Utility -------------------------------------------------

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
  (list 'procedure params (scan-out-defines body) env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-params p) (cadr   p))
(define (procedure-body   p) (caddr  p))
(define (procedure-env    p) (cadddr p))

(define lazy-car (lambda (pair) (pair (lambda (x y) x))))
(define lazy-cdr (lambda (pair) (pair (lambda (x y) y))))
(define lazy-cons (lambda (x y) (lambda (m) (m x y))))

(define primitive-procedures
  (list (list 'car   lazy-car)
        (list 'cdr   lazy-cdr)
        (list 'cons  lazy-cons)
        (list 'null? null?)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)))
(define (primitive-procedure-names)
  (map car primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc)
        (list 'primitive (cadr proc)))
       primitive-procedures))
(define (primitive-implementation proc)
  (cadr proc))

;; Environment ---------------------------------------------

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
          (error "Too few arguments supplied"
                 vars
                 vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (if (eq? (car vals) '*unassigned*)
                 (error 'lookup "Unassigned val" var)
                 (car vals)))
            (else (scan (cdr vars)
                        (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error 'lookup "Unbound variable" var)
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
(define global-env the-global-environment)

(define (self-evaluating? expr)
  (cond ((number? expr) #t)
        ((string? expr) #t)
        ((eq? expr #t)  #t)
        ((eq? expr #f)  #t)
        (else #f)))

(define (create-stream x)
  (if (null? x)
      '()
      (lazy-cons (car x)
                 (create-stream (cdr x)))))
(define (quoted? exp) (tagged-list? exp 'quote))
(define (eval-quote exp)
  (define rest (cadr exp))
  (if (list? rest)
      (create-stream rest)
      (cadr exp)))

(define (variable? exp) (symbol? exp))

(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))
(define (eval-assignment exp env)
  (set-variable-value! 
   (assignment-variable exp)
   (eval-expr (assignment-value exp) env)
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

;; (define (eval-if exp env)
;;   (if (true? (eval-expr (if-predicate exp) env))
;;       (eval-expr (if-consequent  exp) env)
;;       (eval-expr (if-alternative exp) env)))

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (eval-expr (if-consequent exp) env)
      (eval-expr (if-alternative exp) env)))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (eval-sequence exps env)
  (cond ((last-exp? exps)
         (eval-expr (first-exp exps) env))
        (else
         (eval-expr (first-exp exps) env)
         (eval-sequence (rest-exps exps)
                        env))))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false     ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last: COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

(define (list-of-values exps env)
  (if (null? exps)
      '()
      (let ((left (eval-expr (car exps) env)))
        (let ((rest (list-of-values (cdr exps) env)))
          (cons left rest)))))

(define (let? exp) (tagged-list? exp 'let))
(define (let->combination exp)
  (define params (map car (cadr exp)))
  (define args (map cadr (cadr exp)))
  (define body (cddr exp))
  (cons (make-lambda params body)
        args))

(define (make-let vars vals body)
  (define (loop vars vals)
    (if (null? vars)
        '()
        (cons (list (car vars)
                    (car vals))
              (loop (cdr vars)
                    (cdr vals)))))
  (cons 'let (cons (loop vars vals) body)))

(define (make-new-body vars vals body)
  (define (loop vars vals)
    (if (null? vars)
        '()
        (cons (list 'set! (car vars) (car vals))
              (loop (cdr vars) (cdr vals)))))
  (append (loop vars vals) body))

(define (scan-out-defines body)
  (define vars (map cadr  (filter definition? body)))
  (define vals (map caddr (filter definition? body)))
  (define exps
    (filter (lambda (x) (not (definition? x))) body))
  (define new-body
    (make-new-body vars vals exps))
  (if (null? vars)
      body
      (list
        (make-let vars
                  (map (lambda (x) 
                        (quote '*unassigned*))
                       vals)
                  new-body))))

(define (letrec? exp) (tagged-list? exp 'letrec))
(define (letrec-vars exp) (map car  (cadr exp)))
(define (letrec-vals exp) (map cadr (cadr exp)))
(define (letrec-body exp) (cddr exp))
(define (letrec->let exp)
  (define vars (letrec-vars exp))
  (define vals (letrec-vals exp))
  (define body (letrec-body exp))
  (define new-body (make-new-body vars vals body))
  (make-let vars
            (map (lambda (x) (quote '*unassigned*)) vals)
            new-body))

;; Lazy ----------------------------------------------------

(define (delay-it exp env)
  (list 'thunk exp env))
(define (thunk? obj) (tagged-list? obj 'thunk))
(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk) 
  (cadr evaluated-thunk))

;; (define (force-it obj)
;;   (if (thunk? obj)
;;       (actual-value (thunk-exp obj)
;;                     (thunk-env obj))
;;       obj))

(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result
                (actual-value 
                 (thunk-exp obj)
                 (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           ;; replace exp with its value:
           (set-car! (cdr obj) result) 
           ;; forget unneeded env:
           (set-cdr! (cdr obj) '())
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))

(define (actual-value exp env)
  (force-it (eval-expr exp env)))

(define (list-of-arg-values exps env)
  (if (null? exps)
      '()
      (cons (actual-value (car exps) env)
            (list-of-arg-values 
             (cdr exps)
             env))))

(define (list-of-delayed-args exps env)
  (if (null? exps)
      '()
      (cons (delay-it (car exps) env)
            (list-of-delayed-args 
             (cdr exps)
             env))))

;; Eval ----------------------------------------------------

(define (eval-expr exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable?   exp) (lookup-variable-value exp env))
        ((quoted?     exp) (eval-quote exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure
          (lambda-parameters exp)
          (lambda-body exp)
          env))
        ((begin? exp)
         (eval-sequence 
          (begin-actions exp) 
          env))
        ((cond? exp) (eval-expr (cond->if exp) env))
        ((let? exp) (eval-expr (let->combination exp) env))
        ((letrec? exp) (eval-expr (letrec->let exp) env))
        ;; ((application? exp)
        ;;  (apply-proc (eval-expr (operator exp) env)
        ;;              (list-of-values (operands exp) env)))
        ((application? exp)
         (apply-proc (actual-value (operator exp) env)
                     (operands exp)
                     env))
        (else
         (error "Unknown expression type: EVAL" exp))))

(define (evalg exp) (eval-expr exp global-env))

;; Apply ---------------------------------------------------

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))

;; (define (apply-proc procedure args)
;;   (cond ((primitive-procedure? procedure)
;;          (apply-primitive-procedure procedure args))
;;         ((compound-procedure? procedure)
;;          (eval-sequence
;;            (procedure-body procedure)
;;            (extend-environment
;;              (procedure-params procedure)
;;              args
;;              (procedure-env procedure))))
;;         (else
;;          (error "Unknown procedure type: APPLY"
;;                 procedure))))

(define (apply-proc procedure args env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values
           args
           env)))  ; changed
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-params procedure)
           (list-of-delayed-args
            args
            env)   ; changed
           (procedure-env procedure))))
        (else
         (error "Unknown procedure type: APPLY"
                procedure))))

;; Exercises -----------------------------------------------

;; 4.25

;; In an applicative-order evaluator, this will produce
;; an infinite loop because all arguments to the unless
;; function will be evaluated. So (factorial 1) will call 
;; (factorial 0) and the base case will be missed.

;; 4.28

;; This is necessary for expressions that pass functions as
;; arguments which are then called. Since it will be delayed,
;; the function will be a thunk, so when it is evaluated,
;; the actual value must be forced so it can be passed to
;; apply. This is because apply does not handle thunks.

;; 4.29

;; (define count 0)
;; (define (id x) (set! count (+ count 1)) x)
;; (define (square x) (* x x))

;; Without memoization:
;; > (square (id 10))
;; 100
;; > count
;; 2

;; With memoization:
;; > (square (id 10))
;; 100
;; > count
;; 1

;; 4.33

;; (define lazy-car (lambda (pair) (pair (lambda (x y) x))))
;; (define lazy-cdr (lambda (pair) (pair (lambda (x y) y))))
;; (define lazy-cons (lambda (x y) (lambda (m) (m x y))))

;; (define (quoted? exp) (tagged-list? exp 'quote))

;; (define (create-stream x)
;;   (if (null? x)
;;       '()
;;       (lazy-cons (car x)
;;                  (create-stream (cdr x)))))

;; (define (eval-quote exp)
;;   (define rest (cadr exp))
;;   (if (list? rest)
;;       (create-stream rest)
;;       (cadr exp)))
