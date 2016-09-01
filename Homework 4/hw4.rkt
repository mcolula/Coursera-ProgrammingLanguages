#lang racket

(provide (all-defined-out))

(define (sequence low high stride)
  (cond [(<= low high)
         (cons low (sequence (+ low stride) high stride))]
        [#t null]))

(define (string-append-map xs suffix)
  (map (lambda(x)(string-append x suffix)) xs))

( define (list-nth-mod xs x) (cond [(< x 0)    (error "list-nth-mod: negative number")]
                                   [(null? xs) (error "list-nth-mod: empty list")]
                                   [#t (car (list-tail xs (remainder x (length xs))))]))

(define funny-number-stream
  (letrec ([f (lambda (x) (if (= 0 (remainder x 5))
                              (cons (* -1 x) (lambda () (f (+ x 1))))
                              (cons x (lambda () (f (+ x 1))))))])
    (lambda () (f 1))))

(define (stream-for-n-steps s n)
  (if (= 0 n)
      null
      (cons (car(s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

(define dan-then-dog
  (letrec ([f (lambda (x) (if x
                              (cons "dog.jpg" (lambda () (f #f)))
                              (cons "dan.jpg" (lambda () (f #t)))))])
    (lambda () (f #f))))

(define (stream-add-zero s)
  (letrec ([f (lambda (x) (cons (cons (car (x)) 0) (lambda()((stream-add-zero (cdr (x)))))))])
    (lambda() (f s))))

(define (cycle-lists xs ys)
  (letrec ([f (lambda (n)
              (cons (cons (list-nth-mod xs n) (list-nth-mod ys n))
                    (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))

(define (vector-assoc v vec)
  (letrec ([f (lambda (n)
                (if (>= n (vector-length vec)) #f
                    (let ([vi (vector-ref vec n)])
                      (cond [(not (pair? vi)) (f (+ n 1))]
                            [(equal? (car vi) v) vi]
                            [#t (f (+ n 1))]))))])
    (f 0)))
 
(define (cached-assoc xs n)
  (letrec ([cache-vec (make-vector n #f)]
           [next 0]
           [f (lambda (x)
                (let ([ans (vector-assoc x cache-vec)])
                 (if ans
                     ans
                     (let ([new-ans (assoc x xs)])
                       new-ans
                       (begin
                         (vector-set! cache-vec next new-ans)
                         (set! next (remainder (+ next 1) n))
                         new-ans)))))])
  f))
                  

