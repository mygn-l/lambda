#lang racket

(define zero (lambda (f) (lambda (x) x)))
(define succ (lambda (n) (lambda (f) (lambda (x) (f ((n f) x))))))

(define numeral_to_int (lambda (n) ((n (lambda (x) (+ x 1))) 0)))

(define add (lambda (n) (lambda (m) (lambda (f) (lambda (x) ((m f) ((n f) x)))))))

(define mult (lambda (n) (lambda (m) (lambda (f) (m (n f))))))

(define two (succ (succ zero)))
(define three (succ two))
(define five ((add two) three))
(define six ((mult two) three))

(numeral_to_int six)
