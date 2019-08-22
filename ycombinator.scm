;; (define fact
;;   ((lambda (f)
;;     (lambda (n)
;;       (if (= n 0)
;;           1
;;           (* n ((f f) (- n 1))))))
;;    (lambda (f)
;;      (lambda (n)
;;        (if (= n 0)
;;            1
;;            (* n ((f f) (- n 1))))))))

;; (define fact
;;   ((lambda (f)
;;      ((lambda (f-arg)
;;         (lambda (n)
;;           (if (= n 0)
;;               1
;;               (* n (f-arg (- n 1))))))
;;       (lambda (arg) ((f f) arg))))
;;    (lambda (f)
;;      ((lambda (f-arg)
;;         (lambda (n)
;;           (if (= n 0)
;;               1
;;               (* n (f-arg (- n 1))))))
;;       (lambda (arg) ((f f) arg))))))

;; (define F
;;   (lambda (f-arg)
;;     (lambda (n)
;;       (if (= n 0)
;;           1
;;           (* n (f-arg (- n 1)))))))

;; (define fact
;;   ((lambda (f)
;;      (F (lambda (arg) ((f f) arg))))
;;    (lambda (f)
;;      (F (lambda (arg) ((f f) arg))))))

(define Y
  (lambda (F)
    ((lambda (x)
       (F (lambda (arg) ((x x) arg))))
     (lambda (x)
       (F (lambda (arg) ((x x) arg)))))))

(define fact-gen
  (lambda (f)
    (lambda (n)
      (if (= n 0)
          1
          (* n (f (- n 1)))))))
(define fact (Y fact-gen))

(define fib-gen
  (lambda (f)
    (lambda (n)
      (if (< n 2)
          n
          (+ (f (- n 1))
             (f (- n 2)))))))
(define fib (Y fib-gen))