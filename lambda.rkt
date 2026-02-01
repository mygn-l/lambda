#lang racket

(define zero (lambda (f) (lambda (x) x))) ;0
(define succ (lambda (n) (lambda (f) (lambda (x) (f ((n f) x)))))) ;n -> n+1
(define one (succ zero)) ;1

(define church_to_int (lambda (n) ((n (lambda (x) (+ x 1))) 0)))
(define int_to_church (lambda (i) (if (= i 0) zero (succ (int_to_church (- i 1))))))

(define add (lambda (n) (lambda (m) (lambda (f) (lambda (x) ((m f) ((n f) x))))))) ;n+m
(define mult (lambda (n) (lambda (m) (lambda (f) (m (n f)))))) ;n*m
(define exp (lambda (n) (lambda (b) ((b (mult n)) one)))) ;n^b

(define pair (lambda (a) (lambda (b) (lambda (s) ((s a) b))))) ;(a, b)
(define first (lambda (p) (p (lambda (a) (lambda (b) a))))) ;p[0]
(define last (lambda (p) (p (lambda (a) (lambda (b) b))))) ;p[1]
(define swap (lambda (p) (lambda (s) ((s (last p)) (first p))))) ;(a, b) -> (b, a)

(define step_pair (lambda (p) ((pair (last p)) (succ (last p))))) ;(<any>, x) -> (x, x+1)
(define pred (lambda (n) (first ((n step_pair) ((pair zero) zero))))) ;n -> n-1
(define sub (lambda (n) (lambda (m) ((m pred) n)))) ;n-m

(define true (lambda (x) (lambda (y) x))) ;true
(define false (lambda (x) (lambda (y) y))) ;false

(define select (lambda (e) (lambda (a) (lambda (b) ((e a) b))))) ;if e then a else b

(define boolean_to_string (lambda (n) (((select n) "true") "false")))

(define and (lambda (a) (lambda (b) (lambda (x) (lambda (y) ((b ((a x) y)) y)))))) ;a and b
(define or (lambda (a) (lambda (b) (lambda (x) (lambda (y) ((b x) ((a x) y))))))) ;a or b
(define not (lambda (a) ((a false) true))) ;not a
(define iszero (lambda (n) ((n (lambda (x) false)) true))) ;n == 0

(define church_equal (lambda (n) (lambda (m) ((and (iszero ((sub n) m))) (iszero ((sub m) n)))))) ;n == m

(define two (int_to_church 2))
(define three (int_to_church 5))
(display (church_to_int ((exp two) three)))
