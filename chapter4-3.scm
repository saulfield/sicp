(define true #t)
(define false #f)

;; amb

(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))

(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

;; analyze

(define (analyze exp)
  (cond ((self-evaluating? exp) (analyze-self-evaluating exp))
        ((list-exp? exp) (analyze-list exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp)))
        ((amb? exp) (analyze-amb exp))
        ((application? exp) (analyze-application exp))
        (else
         (error 'analyze
                "Unknown expression type -- ANALYZE"
                exp))))

(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))

(define (list-exp? exp) (tagged-list? exp 'list))
(define (analyze-list exp)
  (let ((elems (map analyze (cdr exp))))
    (lambda (env succeed fail)
      (define (resolve elem)
        (elem env (lambda (val fail2) val) fail))
      (define vals (map resolve elems))
      (succeed vals fail))))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed qval fail))))

(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env) fail)))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env) fail))))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (pred-value fail2)
               (if (true? pred-value)
                   (cproc env succeed fail2)
                   (aproc env succeed fail2)))
             fail))))

(define (analyze-sequence exps)
  (define (sequentially a b)
    (lambda (env succeed fail)
      (a env
         (lambda (a-value fail2)
           (b env succeed fail2))
         fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc
                            (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error 'analyze-sequence 
               "Empty sequence -- ANALYZE"
               procs))
    (loop (car procs) (cdr procs))))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (define-variable! var val env)
               (succeed 'ok fail2))
             fail))))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (let ((old-value
                     (lookup-variable-value var env)))
                 (set-variable-value! var val env)
                 (succeed 'ok
                          (lambda ()
                            (set-variable-value! var
                                                 old-value
                                                 env)
                            (fail2)))))
             fail))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (fproc env
             (lambda (proc fail2)
               (get-args aprocs
                         env
                         (lambda (args fail3)
                           (execute-application
                            proc args succeed fail3))
                         fail2))
             fail))))

(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
      (succeed '() fail)
      ((car aprocs)
       env
       (lambda (arg fail2)
         (get-args (cdr aprocs)
                   env
                   (lambda (args fail3)
                     (succeed (cons arg args)
                              fail3))
                   fail2))
       fail)))

(define (execute-application proc args succeed fail)
  (cond ((primitive-procedure? proc)
         (succeed (apply-primitive-procedure proc args) fail))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))
          succeed
          fail))
        (else
         (error 'execute-application
                "Unknown procedure type -- EXECUTE-APPLICATION"
                proc))))

(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices)
             env
             succeed
             (lambda ()
               (try-next (cdr choices))))))
      (try-next cprocs))))

;; support functions

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        ((eq? #t exp) true)
        ((eq? #f exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
              (cddr exp))))

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

;; * derived expressions

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

(define (let? exp) (tagged-list? exp 'let))
(define (let->combination exp)
  (define params (map car (cadr exp)))
  (define args (map cadr (cadr exp)))
  (define body (cddr exp))
  (cons (make-lambda params body) args))

;; * testing of predicates

(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

;; * representing procedures

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))

(define (procedure-body p) (caddr p))

(define (procedure-environment p) (cadddr p))

;; * operations on environments

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
          (error "Too many argument supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
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
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variables -- SET!" var)
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
             (set-car! vars val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

;; * procedures

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define (prime? n)
  (let loop ((d 2))
    (cond ((< n (* d d)) #t)
          ((zero? (modulo n d)) #f)
          (else (loop (+ d 1))))))

(define primitive-procedures
  (list (list 'car    car)
        (list 'cdr    cdr)
        (list 'cons   cons)
        (list 'cadr   cadr)
        (list 'caddr  caddr)
        (list 'null?  null?)
        (list 'prime? prime?)
        (list 'not    not)
        (list 'printf printf)
        (list '+      +)
        (list '-      -)
        (list '*      *)
        (list '/      /)
        (list '=      =)))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))

(define global-env (setup-environment))

;; driver loop

(define input-prompt  ";;; Amb-Eval input:")
(define output-prompt ";;; Amb-Eval value:")
(define (prompt-for-input string)
  (newline) (newline) (display string) (display " "))
(define (announce-output string)
  (newline) (display string) (newline))
(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (eq? input 'try-again)
          (try-again)
          (begin
            (newline)
            (display ";;; Starting a new problem")
            (ambeval input
                     global-env
                     (lambda (val next-alternative)
                       (announce-output output-prompt)
                       (user-print val)
                       (internal-loop next-alternative))
                     (lambda ()
                       (announce-output
                        ";;; There are no more values of")
                       (user-print input)
                       (driver-loop)))))))
  (internal-loop
    (lambda ()
      (newline)
      (display ";;; There is no current problem")
      (driver-loop))))

(define (ambeval-repeat exp n)
  (define count 0)
  (define (loop try-again)
    (if (> count 0)
        (try-again)
        (ambeval exp
                 global-env
                 (lambda (val next-alternative)
                   (set! count (+ count 1))
                   (user-print val)
                   (newline)
                   (if (< count n)
                       (loop next-alternative)
                       (newline)))
                 (lambda ()
                   (announce-output "No more values of")
                   (user-print exp)))))
  (loop (lambda ()
          (newline)
          (display "There is no current problem")
          (newline))))

(define definitions
  (list '(define (require p)
           (if (not p) (amb)))
        '(define (an-element-of items)
           (require (not (null? items)))
           (amb (car items)
                (an-element-of (cdr items))))
        '(define (prime-sum-pair list1 list2)
           (let ((a (an-element-of list1))
                 (b (an-element-of list2)))
             (require (prime? (+ a b)))
             (list a b)))
        '(define (an-integer-between low high)
           (define (loop n)
             (require (not (= n high)))
             (amb n (loop (+ n 1))))
           (loop low))
        '(define (an-integer-starting-from n)
           (amb n (an-integer-starting-from (+ n 1))))
        '(define (next triple)
           (let ((a (car   triple))
                 (b (cadr  triple))
                 (c (caddr triple)))
             (cond ((= a b c) (list 1 1 (+ c 1)))
                   ((= a b)   (list 1 (+ b 1) c))
                   (else      (list (+ a 1) b c)))))
        '(define (a-triple-from triple)
           (amb triple
                (a-triple-from (next triple))))
        '(define (pythagorean-triple)
          (let ((triple (a-triple-from '(1 1 1))))
            (let ((i (car   triple))
                  (j (cadr  triple))
                  (k (caddr triple)))
              (require (= (+ (* i i) (* j j)) (* k k)))
              (list i j k))))))

(define (load-definitions defs)
  (if (null? defs)
      'loaded
      (begin
        (ambeval (car defs)
                 global-env
                 (lambda (val fail) 'ok)
                 (lambda () 'failed))
        (load-definitions (cdr defs)))))

(load-definitions definitions)
;; (driver-loop)

;; exercises

;; (prime-sum-pair '(19 27 30) '(11 36 58))

;; 4.35

(define (an-integer-between low high)
  (define (loop n)
    (require (not (= n high)))
    (amb n (loop (+ n 1))))
  (loop low))

;; 4.36

;; (ambeval-repeat
;;   '(define (pythagorean-triple-from low)
;;     (let ((i (an-integer-starting-from low)))
;;       (let ((j (an-integer-starting-from low)))
;;         (let ((k (an-integer-starting-from low)))
;;           (printf "~a ~a ~a~n" i j k)
;;           (require (= (+ (* i i) (* j j))
;;                       (* k k)))
;;           (list i j k)))))
;;   1)

;; (ambeval-repeat '(pythagorean-triple-from 1) 10)

;; output:

;; ...
;; 1 1 19772
;; 1 1 19773
;; 1 1 19774
;; 1 1 19775
;; 1 1 19776
;; 1 1 19777
;; ...

;; use depth first search instead

(define (next triple)
          (let ((a (car   triple))
                (b (cadr  triple))
                (c (caddr triple)))
            (cond ((= a b c) (list 1 1 (+ c 1)))
                  ((= a b)   (list 1 (+ b 1) c))
                  (else      (list (+ a 1) b c)))))

(define (a-triple-from triple)
  (amb triple
       (a-triple-from (next triple))))

(define (pythagorean-triple)
  (let ((triple (a-triple-from '(1 1 1))))
    (let ((i (car   triple))
          (j (cadr  triple))
          (k (caddr triple)))
      (require (= (+ (* i i) (* j j)) (* k k)))
      (list i j k))))
