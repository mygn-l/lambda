#lang racket

(define zero_nat (lambda (f) (lambda (x) x))) ;0
(define succ (lambda (n) (lambda (f) (lambda (x) (f ((n f) x)))))) ;n -> n+1
(define one_nat (succ zero_nat)) ;1

(define church_nat_to_lisp (lambda (n) ((n (lambda (x) (+ x 1))) 0)))
(define lisp_to_church_nat (lambda (i) (if (= i 0) zero_nat (succ (lisp_to_church_nat (- i 1))))))

(define add_nat (lambda (n) (lambda (m) (lambda (f) (lambda (x) ((m f) ((n f) x))))))) ;n+m
(define mult_nat (lambda (n) (lambda (m) (lambda (f) (m (n f)))))) ;n*m
(define exp_nat (lambda (n) (lambda (b) ((b (mult_nat n)) one_nat)))) ;n^b

(define pair (lambda (a) (lambda (b) (lambda (s) ((s a) b))))) ;(a, b)
(define first (lambda (p) (p (lambda (a) (lambda (b) a))))) ;p[0]
(define last (lambda (p) (p (lambda (a) (lambda (b) b))))) ;p[1]
(define swap (lambda (p) (lambda (s) ((s (last p)) (first p))))) ;(a, b) -> (b, a)

(define step_pair_nat (lambda (p) ((pair (last p)) (succ (last p))))) ;(<any>, x) -> (x, x+1)
(define pred_nat (lambda (n) (first ((n step_pair_nat) ((pair zero_nat) zero_nat))))) ;n -> n-1
(define sub_nat (lambda (n) (lambda (m) ((m pred_nat) n)))) ;n-m

(define true (lambda (x) (lambda (y) x))) ;true
(define false (lambda (x) (lambda (y) y))) ;false

(define select (lambda (e) (lambda (a) (lambda (b) ((e a) b))))) ;if e then a else b

(define boolean_to_string (lambda (n) (((select n) "true") "false")))

(define and (lambda (a) (lambda (b) (lambda (x) (lambda (y) ((b ((a x) y)) y)))))) ;a and b
(define or (lambda (a) (lambda (b) (lambda (x) (lambda (y) ((b x) ((a x) y))))))) ;a or b
(define not (lambda (a) ((a false) true))) ;not a

(define iszero_nat (lambda (n) ((n (lambda (x) false)) true))) ;n == 0
(define church_nat_equal (lambda (n) (lambda (m) ((and (iszero_nat ((sub_nat n) m))) (iszero_nat ((sub_nat m) n)))))) ;n == m
(define church_nat_geq (lambda (n) (lambda (m) (iszero_nat ((sub_nat m) n)))))

(define church_int (lambda (a) (lambda (b) ((pair a) b))))
(define zero_int ((church_int zero_nat) zero_nat))
(define one_int ((church_int one_nat) zero_nat))
(define minus_one_int ((church_int zero_nat) one_nat))

(define church_int_to_lisp (lambda (n)
  (((select ((church_nat_geq (first n)) (last n)))
    (church_nat_to_lisp ((sub_nat (first n)) (last n))))
    (- (church_nat_to_lisp ((sub_nat (last n)) (first n)))))))
(define lisp_to_church_int (lambda (i)
  (if (>= i 0)
      ((church_int (lisp_to_church_nat i)) zero_nat)
      ((church_int zero_nat) (lisp_to_church_nat (- i))))))

(define add_int (lambda (n) (lambda (m)
  ((church_int ((add_nat (first n)) (first m))) ((add_nat (last n)) (last m))))))
(define neg_int (lambda (n) (swap n)))
(define sub_int (lambda (n) (lambda (m) ((add_int n) (neg_int m)))))
(define mult_int (lambda (n) (lambda (m)
  ((church_int ((add_nat ((mult_nat (first n)) (first m))) ((mult_nat (last n)) (last m))))
   ((add_nat ((mult_nat (first n)) (last m))) ((mult_nat (last n)) (first m)))))))

(define church_rational (lambda (a) (lambda (b) ((pair a) b))))
(define zero_rat ((church_rational zero_int) one_int))
(define one_rat ((church_rational one_int) one_int))

(define rational_to_string (lambda (q)
  (string-append
    (number->string (church_int_to_lisp (first q)))
    "/"
    (number->string (church_int_to_lisp (last q))))))

(define add_rat (lambda (p) (lambda (q) ((church_rational
  ((add_int ((mult_int (first p)) (last q))) ((mult_int (first q)) (last p))))
  ((mult_int (last p)) (last q))))))
(define neg_rat (lambda (p) ((church_rational (neg_int (first p))) (last p))))
(define sub_rat (lambda (p) (lambda (q) ((add_rat p) (neg_rat q)))))
(define mult_rat (lambda (p) (lambda (q) ((church_rational
  ((mult_int (first p)) (first q)))
  ((mult_int (last p)) (last q))))))
(define inverse_rat (lambda (p) (swap p)))
(define div_rat (lambda (p) (lambda (q) ((mult_rat p) (inverse_rat q)))))







