(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
        (cadr val)
        (error "Unknown operation: ASSEMBLE" symbol))))

(define (operation-exp? exp)
  (and (pair? exp)
       (tagged-list? (car exp) 'op)))

(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))

(define (operation-exp-operands operation-exp)
  (cdr operation-exp))

(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        (aprocs
         (map (lambda (e)
                (if (not (label-exp? e))
                    (make-primitive-exp e machine labels)
                    (error #f "Operands cannot be labels" e)))
              (operation-exp-operands exp))))
    (lambda () 
      (apply op (map (lambda (p) (p)) aprocs)))))

(define (register-exp? exp) (tagged-list? exp 'reg))
(define (constant-exp? exp) (tagged-list? exp 'const))
(define (label-exp? exp)    (tagged-list? exp 'label))

(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
         (let ((c (cadr exp)))
           (lambda () c)))
        ((label-exp? exp)
         (let ((insts (lookup-label labels (cadr exp))))
           (lambda () insts)))
        ((register-exp? exp)
         (let ((r (get-register machine (cadr exp))))
           (lambda () (get-contents r))))
        (else (error "Unknown expression type: ASSEMBLE" exp))))

(define (make-perform inst machine labels operations pc)
  (let ((action (cdr inst)))
    (if (operation-exp? action)
        (let ((action-proc
               (make-operation-exp
                action
                machine
                labels
                operations)))
          (lambda ()
            (action-proc)
            (advance-pc pc)))
        (error "Bad PERFORM instruction: ASSEMBLE" inst))))

(define (make-save inst machine stack pc)
  (let ((reg (get-register machine (cadr inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine (cadr inst))))
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc))))

(define (make-goto inst machine labels pc)
  (let ((dest (cadr inst)))
    (cond ((label-exp? dest)
           (let ((insts (lookup-label labels (cadr dest))))
             (lambda ()
               (set-contents! pc insts))))
          ((register-exp? dest)
           (let ((reg (get-register machine (cadr dest))))
             (lambda ()
               (set-contents! pc (get-contents reg)))))
          (else (error "Bad GOTO instruction: ASSEMBLE" inst)))))

(define (branch-dest branch-instruction)
  (cadr branch-instruction))

(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
        (let ((insts (lookup-label labels (cadr dest))))
          (lambda ()
            (if (get-contents flag)
                (set-contents! pc insts)
                (advance-pc pc))))
        (error "Bad BRANCH instruction: ASSEMBLE" inst))))

(define (make-test inst machine labels operations flag pc)
  (let ((condition (cdr inst)))
    (if (operation-exp? condition)
        (let ((condition-proc
               (make-operation-exp
                condition 
                machine
                labels
                operations)))
          (lambda () 
            (set-contents! flag (condition-proc))
            (advance-pc pc)))
        (error "Bad TEST instruction: ASSEMBLE" inst))))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))
(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))

(define (make-assign inst machine labels operations pc)
  (let ((target (get-register machine (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
    (let ((value-proc
           (if (operation-exp? value-exp)
               (make-operation-exp
                value-exp 
                machine
                labels
                operations)
               (make-primitive-exp
                (car value-exp)
                machine
                labels))))
      (lambda ()   ; execution procedure for assign
        (set-contents! target (value-proc))
        (advance-pc pc)))))

(define (make-execution-procedure inst labels machine pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
         (make-assign inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (make-branch inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine stack pc))
        ((eq? (car inst) 'restore)
         (make-restore inst machine stack pc))
        ((eq? (car inst) 'perform)
         (make-perform inst machine labels ops pc))
        (else (error "Unknown instruction type: ASSEMBLE" inst))))

(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
        (cdr val)
        (error "Undefined label: ASSEMBLE"  label-name))))

(define (make-label-entry label-name insts)
  (cons label-name insts))

(define (make-instruction text) (cons text '()))
(define (instruction-text inst) (car inst))
(define (instruction-execution-proc inst) (cdr inst))
(define (set-instruction-execution-proc! inst proc)
  (set-cdr! inst proc))

(define (extract-labels text)
  (if (null? text)
      (cons '() '())
      (let ((result (extract-labels (cdr text))))
        (let ((insts (car result))
              (labels (cdr result)))
          (let ((next-inst (car text)))
            (if (symbol? next-inst)
                (if (assoc next-inst labels)
                    (error 'extract "Label already used" next-inst)
                    (cons insts
                      (cons (make-label-entry next-inst insts)
                            labels)))
                (cons (cons (make-instruction next-inst)
                            insts)
                      labels)))))))

(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)))
    (for-each
     (lambda (inst)
       (set-instruction-execution-proc!
        inst
        (make-execution-procedure
         (instruction-text inst) 
         labels
         machine
         pc
         flag
         stack
         ops)))
     insts)))

(define (assemble controller-text machine)
  (let ((result (extract-labels controller-text)))
    (let ((insts (car result))
          (labels (cdr result)))
      (update-insts! insts labels machine)
      insts)))

(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

(define (start machine) (machine 'start))

(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))

(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value)
  'done)

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '()))
    (let ((the-ops
           (list
            (list 'initialize-stack
                  (lambda () 
                    (stack 'initialize)))))
          (register-table
           (list (list 'pc pc) 
                 (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
                  (cons
                   (list name
                         (make-register name))
                   register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register:" name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                ((instruction-execution-proc 
                  (car insts)))
                (execute)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) 
                  ; (display seq) (newline)
                 (set! the-instruction-sequence seq)))
              ((eq? message 'install-operations)
               (lambda (ops) 
                 (set! the-ops (append the-ops ops))))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register)      lookup-register)
              ((eq? message 'operations)        the-ops)
              ((eq? message 'stack)             stack)
              (else (error "Unknown request: MACHINE" message))))
      dispatch)))

(define (pop stack) (stack 'pop))
(define (push stack value) ((stack 'push) value))

(define (make-stack)
  (let ((s '()))
    (define (push x)
      (set! s (cons x s)))
    (define (pop)
      (if (null? s)
          (error "Empty stack: POP" s)
          (let ((top (car s)))
            (set! s (cdr s))
            top)))
    (define (initialize)
      (set! s '())
      'done)
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize) (initialize))
            (else (error "Unknown request: STACK" message))))
    dispatch))

(define (get-contents register) (register 'get))
(define (set-contents! register value) ((register 'set) value))

(define (make-register name)
  (let ((contents '*unassigned*))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value) 
               (set! contents value)))
            (else (error "Unknown request: REGISTER" message))))
    dispatch))

(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each (lambda (register-name)
                ((machine 'allocate-register) register-name))
              register-names)
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

;; run

(define vector-machine
  (make-machine
   '(a b c x y val free the-cars the-cdrs)
   (list (list 'rem remainder)
         (list 'make-vector make-vector)
         (list 'vector-set! vector-set!)
         (list 'vector-ref vector-ref)
         (list '+ +)
   )
   '(
     ; initialize memory
     (assign the-cars (op make-vector) (const 100))
     (assign the-cdrs (op make-vector) (const 100))
     (assign free (const 0))

     ; allocate a new cons pair using vals in a and b
     (perform (op vector-set!) (reg the-cars) (reg free) (reg a))
     (perform (op vector-set!) (reg the-cdrs) (reg free) (reg b))
     (assign c (reg free))
     (assign free (op +) (reg free) (const 1))

     ; get car and cdr of the cons pair and add them together
     (assign x (op vector-ref) (reg the-cars) (reg c))  ; car
     (assign y (op vector-ref) (reg the-cdrs) (reg c))  ; cdr
     (assign val (op +) (reg x) (reg y))
    )))

(set-register-contents! vector-machine 'a 1)
(set-register-contents! vector-machine 'b 2)
(start vector-machine)
(printf "~a~n" (get-register-contents vector-machine 'val))

; Exercises

; 5.20

; +---+---+   +---+---+
; | * | * |-->| * |NIL|
; +---+---+   +---+---+
;   |           |
;   +-----------+
;   |
;   v
; +---+---+
; | * | * |
; +---+---+
;   |   |
;   v   v
;   1   2

; index      0  1  2  3  4
;          +--+--+--+--+--+
; the-cars |  |n1|p1|p1|  |
;          +--+--+--+--+--+
; the-cdrs |  |n2|p3|e0|  |
;          +--+--+--+--+--+