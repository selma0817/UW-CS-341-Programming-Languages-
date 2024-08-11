
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;; sequence 
(define (sequence low high stride)
           (if(> low high)
           null
           (cons low (sequence (+ low stride) high stride))))


;; string-append-map
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))


;; list-nth-mod
(define (list-nth-mod xs n)
  (cond[(< n 0) (error "list-nth-mod: negative number")]
       [(null? xs) (error "list-nth-mod: empty list")]
       [#t (let ([i (remainder n (length xs))])
           (car (list-tail xs i)))]))


;; stream-for-n-steps
(define (stream-for-n-steps s n)
  (if (= 0 n)
      null
      (let ([pr (s)])
        (cons (car pr) (stream-for-n-steps(cdr pr) (- n 1))))))


;; funny-number-stream, stream is a thunk when called produced a pair.
;; use letrec to define local helper function
(define funny-number-stream
  (letrec
      ([f (lambda(x)
          (cons (if (= 0 (remainder x 5 )) (- x) x)
                (lambda () (f (+ x 1)))))])
    (lambda() (f 1))))


;; dan-then-dog

(define dan-then-dog
  (letrec
      ([f (lambda(b)
          (if b
              (cons "dan.jpg" (lambda() (f #f)))
              (cons "dog.jpg" (lambda() (f #t)))))])
    (lambda()(f #t))))
          
;; stream-add-zero
(define (stream-add-zero s)
  (lambda()
    (let ([pr (s)])
      (cons (cons 0 (car pr)) (stream-add-zero (cdr pr))))))
  

;; cycle-lists 
(define (cycle-lists xs ys)
  (letrec
      ([f (lambda(n)      
           (let ([x (list-nth-mod xs n)]
                 [y (list-nth-mod ys n)])
            (cons (cons x y) (lambda () (f (+ n 1))))))])
    (lambda() (f 0))))
            
                 
;; vector-assoc
(define (vector-assoc v vec)
  (letrec
      ([f (lambda (x)
            (if (>= x (vector-length vec))
                #f
                (let ([ith (vector-ref vec x)])
                  (if (and (pair? ith) (equal? (car ith) v))
                  ith
                  (f (+ x 1))))))])
     (f 0)))



;; cached-assoc
(define (cached-assoc xs n)
  (letrec([memo (make-vector n #f)]
          [idx 0])
    (lambda (v)
      (or (vector-assoc v memo)
          (let ([new-ans (assoc v xs)])
            ;; use and to handle #f of assoc
            ;; use begin to set map, update idx, and ret ans
            (and new-ans
                  (begin
                    (vector-set! memo (remainder idx n) new-ans)
                       (set! idx (+ idx 1))
                       new-ans)))))))


;; while-less
(define-syntax while-less
  (syntax-rules(do)
    [(while-less e1 do e2)
     (letrec([val-e1 e1]
             [loop (lambda() (if (>= e2 val-e1) #t (loop)))])
       (loop))]))
  