(load "chapter4-1.scm")
(load "chapter5-2.scm")

(define (compile exp target linkage)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
        ((quoted? exp) 
         (compile-quoted exp target linkage))
        ((variable? exp)
         (compile-variable exp target linkage))
        ((assignment? exp)
         (compile-assignment exp target linkage))
        ((definition? exp)
         (compile-definition exp target linkage))
        ((if? exp)
         (compile-if exp target linkage))
        ((lambda? exp)
         (compile-lambda exp target linkage))
        ((begin? exp)
         (compile-sequence (begin-actions exp) target linkage))
        ((cond? exp) 
         (compile (cond->if exp) target linkage))
        ((application? exp)
         (compile-application exp target linkage))
        (else
         (error "Unknown expression type: COMPILE" exp))))

; Compiler utility functions

(define all-regs '(env proc val argl continue))

(define label-counter 0)

(define (new-label-number)
  (set! label-counter (+ 1 label-counter))
  label-counter)

(define (make-label name)
  (string->symbol
   (string-append 
    (symbol->string name)
    (number->string (new-label-number)))))

(define (make-compiled-procedure entry env)
  (list 'compiled-procedure entry env))
(define (compiled-procedure? proc)
  (tagged-list? proc 'compiled-procedure))
(define (compiled-procedure-entry c-proc) 
  (cadr c-proc))
(define (compiled-procedure-env c-proc)
  (caddr c-proc))

(define (make-instruction-sequence 
         needs modifies statements)
  (list needs modifies statements))

(define (empty-instruction-sequence)
  (make-instruction-sequence '() '() '()))

(define (compile-linkage linkage)
  (cond ((eq? linkage 'return)
         (make-instruction-sequence 
          '(continue)
          '()
          '((goto (reg continue)))))
        ((eq? linkage 'next)
         (empty-instruction-sequence))
        (else
         (make-instruction-sequence '() '()
          `((goto (label ,linkage)))))))

(define (end-with-linkage 
         linkage instruction-sequence)
  (preserving '(continue)
   instruction-sequence
   (compile-linkage linkage)))

(define (compile-self-evaluating 
         exp target linkage)
  (end-with-linkage
   linkage (make-instruction-sequence 
            '()
            (list target)
            `((assign ,target (const ,exp))))))

(define (compile-quoted exp target linkage)
  (end-with-linkage
   linkage
   (make-instruction-sequence
    '()
    (list target)
    `((assign 
       ,target
       (const ,(text-of-quotation exp)))))))

(define (compile-variable
         exp target linkage)
  (end-with-linkage 
   linkage
   (make-instruction-sequence 
    '(env)
    (list target)
    `((assign ,target
              (op lookup-variable-value)
              (const ,exp)
              (reg env))))))

(define (compile-assignment 
         exp target linkage)
  (let ((var (assignment-variable exp))
        (get-value-code
         (compile (assignment-value exp) 
                  'val
                  'next)))
    (end-with-linkage 
     linkage
     (preserving 
      '(env)
      get-value-code
      (make-instruction-sequence
       '(env val)
       (list target)
       `((perform (op set-variable-value!)
                  (const ,var)
                  (reg val)
                  (reg env))
         (assign ,target (const ok))))))))

(define (compile-definition 
         exp target linkage)
  (let ((var (definition-variable exp))
        (get-value-code
         (compile (definition-value exp)
                  'val
                  'next)))
    (end-with-linkage
     linkage
     (preserving 
      '(env)
      get-value-code
      (make-instruction-sequence
       '(env val)
       (list target)
       `((perform (op define-variable!)
                  (const ,var)
                  (reg val)
                  (reg env))
         (assign ,target (const ok))))))))

(define (compile-if exp target linkage)
  (let ((t-branch (make-label 'true-branch))
        (f-branch (make-label 'false-branch))
        (after-if (make-label 'after-if)))
    (let ((consequent-linkage
           (if (eq? linkage 'next) 
               after-if
               linkage)))
      (let ((p-code 
             (compile (if-predicate exp)
                      'val
                      'next))
            (c-code
             (compile (if-consequent exp) 
                      target 
                      consequent-linkage))
            (a-code
             (compile (if-alternative exp)
                      target
                      linkage)))
        (preserving 
         '(env continue)
         p-code
         (append-instruction-sequences
          (make-instruction-sequence 
           '(val) 
           '()
           `((test (op false?) (reg val))
             (branch (label ,f-branch))))
          (parallel-instruction-sequences
           (append-instruction-sequences 
            t-branch c-code)
           (append-instruction-sequences
            f-branch a-code))
          after-if))))))

(define (compile-sequence seq target linkage)
  (if (last-exp? seq)
      (compile (first-exp seq) target linkage)
      (preserving '(env continue)
       (compile (first-exp seq) target 'next)
       (compile-sequence (rest-exps seq)
                         target
                         linkage))))

(define (compile-lambda exp target linkage)
  (let ((proc-entry 
         (make-label 'entry))
        (after-lambda 
         (make-label 'after-lambda)))
    (let ((lambda-linkage
           (if (eq? linkage 'next)
               after-lambda
               linkage)))
      (append-instruction-sequences
       (tack-on-instruction-sequence
        (end-with-linkage 
         lambda-linkage
         (make-instruction-sequence 
          '(env)
          (list target)
          `((assign 
             ,target
             (op make-compiled-procedure)
             (label ,proc-entry)
             (reg env)))))
        (compile-lambda-body exp proc-entry))
       after-lambda))))

(define (compile-lambda-body exp proc-entry)
  (let ((formals (lambda-parameters exp)))
    (append-instruction-sequences
     (make-instruction-sequence 
      '(env proc argl)
      '(env)
      `(,proc-entry
        (assign env 
                (op compiled-procedure-env)
                (reg proc))
        (assign env
                (op extend-environment)
                (const ,formals)
                (reg argl)
                (reg env))))
     (compile-sequence (lambda-body exp)
                       'val
                       'return))))
(define (compile-application 
         exp target linkage)
  (let ((proc-code 
         (compile (operator exp) 'proc 'next))
        (operand-codes
         (map (lambda (operand)
                (compile operand 'val 'next))
              (operands exp))))
    (preserving 
     '(env continue)
     proc-code
     (preserving 
      '(proc continue)
      (construct-arglist operand-codes)
      (compile-procedure-call 
       target
       linkage)))))

(define (construct-arglist operand-codes)
    (let ((operand-codes 
         (reverse operand-codes)))
    (if (null? operand-codes)
        (make-instruction-sequence 
         '() 
         '(argl)
         '((assign argl (const ()))))
        (let ((code-to-get-last-arg
               (append-instruction-sequences
                (car operand-codes)
                (make-instruction-sequence 
                 '(val)
                 '(argl)
                 '((assign argl
                           (op list)
                           (reg val)))))))
          (if (null? (cdr operand-codes))
              code-to-get-last-arg
              (preserving 
               '(env)
               code-to-get-last-arg
               (code-to-get-rest-args
                (cdr operand-codes))))))))

(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
         (preserving 
          '(argl)
          (car operand-codes)
          (make-instruction-sequence 
           '(val argl)
           '(argl)
           '((assign argl
                     (op cons)
                     (reg val)
                     (reg argl)))))))
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (preserving 
         '(env)
         code-for-next-arg
         (code-to-get-rest-args 
          (cdr operand-codes))))))

(define (compile-procedure-call
         target linkage)
  (let ((primitive-branch 
         (make-label 'primitive-branch))
        (compiled-branch 
         (make-label 'compiled-branch))
        (after-call
         (make-label 'after-call)))
    (let ((compiled-linkage
           (if (eq? linkage 'next)
               after-call
               linkage)))
      (append-instruction-sequences
       (make-instruction-sequence 
        '(proc)
        '()
        `((test 
           (op primitive-procedure?)
           (reg proc))
          (branch 
           (label ,primitive-branch))))
       (parallel-instruction-sequences
        (append-instruction-sequences
         compiled-branch
         (compile-proc-appl 
          target
          compiled-linkage))
        (append-instruction-sequences
         primitive-branch
         (end-with-linkage
          linkage
          (make-instruction-sequence
           '(proc argl)
           (list target)
           `((assign 
              ,target
              (op apply-primitive-procedure)
              (reg proc)
              (reg argl)))))))
       after-call))))

(define (compile-proc-appl target linkage)
  (cond ((and (eq? target 'val)
              (not (eq? linkage 'return)))
         (make-instruction-sequence 
          '(proc)
          all-regs
          `((assign continue (label ,linkage))
            (assign 
             val 
             (op compiled-procedure-entry)
             (reg proc))
            (goto (reg val)))))
        ((and (not (eq? target 'val))
              (not (eq? linkage 'return)))
         (let ((proc-return 
                (make-label 'proc-return)))
           (make-instruction-sequence 
            '(proc)
            all-regs
            `((assign continue 
                      (label ,proc-return))
              (assign 
               val 
               (op compiled-procedure-entry)
               (reg proc))
              (goto (reg val))
              ,proc-return
              (assign ,target (reg val))
              (goto (label ,linkage))))))
        ((and (eq? target 'val)
              (eq? linkage 'return))
         (make-instruction-sequence 
          '(proc continue) 
          all-regs
          '((assign 
             val 
             (op compiled-procedure-entry)
             (reg proc))
            (goto (reg val)))))
        ((and (not (eq? target 'val))
              (eq? linkage 'return))
         (error "return linkage, 
                 target not val: COMPILE"
                target))))

(define (registers-needed s)
  (if (symbol? s) '() (car s)))
(define (registers-modified s)
  (if (symbol? s) '() (cadr s)))
(define (statements s)
  (if (symbol? s) (list s) (caddr s)))

(define (needs-register? seq reg)
  (memq reg (registers-needed seq)))
(define (modifies-register? seq reg)
  (memq reg (registers-modified seq)))

(define (append-instruction-sequences . seqs)
  (define (append-2-sequences seq1 seq2)
    (make-instruction-sequence
     (list-union 
      (registers-needed seq1)
      (list-difference 
       (registers-needed seq2)
       (registers-modified seq1)))
     (list-union
      (registers-modified seq1)
      (registers-modified seq2))
     (append (statements seq1)
             (statements seq2))))
  (define (append-seq-list seqs)
    (if (null? seqs)
        (empty-instruction-sequence)
        (append-2-sequences 
         (car seqs)
         (append-seq-list (cdr seqs)))))
  (append-seq-list seqs))

(define (list-union s1 s2)
  (cond ((null? s1) s2)
        ((memq (car s1) s2)
         (list-union (cdr s1) s2))
        (else
         (cons (car s1)
               (list-union (cdr s1) s2)))))

(define (list-difference s1 s2)
  (cond ((null? s1) '())
        ((memq (car s1) s2)
         (list-difference (cdr s1) s2))
        (else 
         (cons (car s1)
               (list-difference (cdr s1)
                                s2)))))

(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ((first-reg (car regs)))
        (if (and 
             (needs-register? seq2 first-reg)
             (modifies-register? seq1 
                                 first-reg))
            (preserving 
             (cdr regs)
             (make-instruction-sequence
              (list-union 
               (list first-reg)
               (registers-needed seq1))
              (list-difference
               (registers-modified seq1)
               (list first-reg))
              (append `((save ,first-reg))
                      (statements seq1)
                      `((restore ,first-reg))))
             seq2)
            (preserving 
             (cdr regs)
             seq1
             seq2)))))

(define (tack-on-instruction-sequence 
         seq body-seq)
  (make-instruction-sequence
   (registers-needed seq)
   (registers-modified seq)
   (append (statements seq)
           (statements body-seq))))

(define (parallel-instruction-sequences 
         seq1 seq2)
  (make-instruction-sequence
   (list-union (registers-needed seq1)
               (registers-needed seq2))
   (list-union (registers-modified seq1)
               (registers-modified seq2))
   (append (statements seq1)
           (statements seq2))))

; run

(define default-ops
  (list (list 'make-compiled-procedure make-compiled-procedure)
         (list 'lookup-variable-value lookup-variable-value)
         (list 'set-variable-value! set-variable-value!)
         (list 'define-variable! define-variable!)
         (list 'false? false?)
         (list 'compiled-procedure-env compiled-procedure-env)
         (list 'extend-environment extend-environment)
         (list 'list list)
         (list 'cons cons)
         (list 'primitive-procedure? primitive-procedure?)
         (list 'apply-primitive-procedure apply-primitive-procedure)
         (list 'compiled-procedure-entry compiled-procedure-entry)
         (list 'get-global-env get-global-env)
         (list 'append append)))

(define (compile-to-machine code)
  (let ((compiled-seq (compile code 'val 'next)))
    (define new-seq
      (append-instruction-sequences
        (make-instruction-sequence
          '()
          '()
          '((assign env (op get-global-env))))
        compiled-seq))
    (pretty-print new-seq)
    (make-machine all-regs default-ops (caddr new-seq))))

; (define code
;   '(begin 
;     (define (factorial n)
;       (if (= n 1)
;           1
;           (* (factorial (- n 1)) n)))
;     (factorial 5)))

; (define code
;   '(define (factorial n)
;     (if (= n 1)
;         1
;         (* (factorial (- n 1)) n))))

(define code
  '(define (factorial-alt n)
    (if (= n 1)
        1
        (* n (factorial-alt (- n 1))))))

(define machine (compile-to-machine code))
(start machine)
(pretty-print (get-register-contents machine 'val))


; 5.33

; $ diff fact1 fact2
; 33,35c33
; <    (save continue) (save proc)
; <    (assign val (op lookup-variable-value) (const n) (reg env))
; <    (assign argl (op list) (reg val)) (save argl)
; ---
; >    (save continue) (save proc) (save env)
; 39c37
; <      (const factorial)
; ---
; >      (const factorial-alt)
; 67c65,66
; <    after-call9 (restore argl)
; ---
; >    after-call9 (assign argl (op list) (reg val)) (restore env)
; >    (assign val (op lookup-variable-value) (const n) (reg env))
; 82c81
; <      (const factorial)
; ---
; >      (const factorial-alt)

; first version: one extra set of save/restore because we need to save the argument list before evaluating the recursive call

; 5.34

; 5.35

; (define (f x)
;   (+ x (g (+ x 2))))

; 5.36

; (define (construct-arglist operand-codes)
;   (let ((operand-codes operand-codes))
;     (if (null? operand-codes)
;         (make-instruction-sequence 
;          '() 
;          '(argl)
;          '((assign argl (const ()))))
;         (let ((code-to-get-last-arg
;                (append-instruction-sequences
;                 (car operand-codes)
;                 (make-instruction-sequence 
;                  '(val)
;                  '(argl)
;                  '((assign argl
;                            (op list)
;                            (reg val)))))))
;           (if (null? (cdr operand-codes))
;               code-to-get-last-arg
;               (preserving 
;                '(env)
;                code-to-get-last-arg
;                (code-to-get-rest-args
;                 (cdr operand-codes))))))))

; (define (code-to-get-rest-args operand-codes)
;   (let ((code-for-next-arg
;          (preserving 
;           '(argl)
;           (car operand-codes)
;           (make-instruction-sequence 
;            '(val argl)
;            '(argl)
;            '((assign val (op list) (reg val))
;              (assign argl
;                      (op append)
;                      (reg argl)
;                      (reg val)))))))
;     (if (null? (cdr operand-codes))
;         code-for-next-arg
;         (preserving 
;          '(env)
;          code-for-next-arg
;          (code-to-get-rest-args 
;           (cdr operand-codes))))))

; 5.37

; (define (preserving regs seq1 seq2)
;   (if (null? regs)
;       (append-instruction-sequences seq1 seq2)
;       (let ((first-reg (car regs)))
;             (preserving 
;              (cdr regs)
;              (make-instruction-sequence
;               (list-union 
;                (list first-reg)
;                (registers-needed seq1))
;               (list-difference
;                (registers-modified seq1)
;                (list first-reg))
;               (append `((save ,first-reg))
;                       (statements seq1)
;                       `((restore ,first-reg))))
;              seq2))))

; before: 6 save/restores
; after: 41 save/restores

; (save argl)
; (save continue)
; (assign val (op lookup-variable-value) (const n) (reg env))
; (restore continue) 
; (restore argl)

; 5.38
