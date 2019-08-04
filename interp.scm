(load "pmatch.scm")

(define eval-exp
  (lambda (exp env)
    (pmatch exp
      [,n (guard (number? n)) n]
      [,x (guard (symbol? x)) (env x)]
      [(lambda (,def-sym) ,body)
       (lambda (arg)
        (eval-exp body (lambda (ref-sym)
                        (if (eq? ref-sym def-sym)
                            arg
                            (env ref-sym)))))]
      [(,operator ,operand)
       ((eval-exp operator env)
        (eval-exp operand env))])))

(define initial-env
  (lambda (x)
    (error 'lookup "unbound variable" x)))

(eval-exp '((lambda (x) x) 5) initial-env)
