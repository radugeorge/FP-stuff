#lang lazy

; Number representation
(define (zero f)
  (lambda (x)
    x))

(define (one f)
  (lambda (x)
    (f x)))

(define (two f)
  (lambda (x)
    (f (f x))))

(define (three f)
  (lambda (x)
    (f (f (f x)))))

; Increment
(define (incr fn)
  (lambda (f)
    (lambda (x)
      (f ((fn f) x)))))

; Decrement
(define (decr fn)
  (lambda (f)
    (lambda (x)
      (((fn (lambda (g)
              (lambda (h)
                (h (g f))))) (lambda (u) x)) (lambda (x) x)))))

; Add
(define (add fm)
  (lambda (fn)
    ((fn incr) fm)))

; Substract
(define (sub fm)
  (lambda (fn)
    ((fn decr) fm)))

; Multiply
(define (mul fm)
  (lambda (fn)
    (lambda (x)
      (fm (fn x)))))

; Booleans
(define (true x)
  (lambda (y) x))

(define (false x)
  (lambda (y) y))

(define (ifte bool)
  (lambda (t)
    (lambda (f)
      ((bool t) f))))

(define (is-zero fn)
  (fn (lambda (u) false)) true)

(define (fact fn)
  (((ifte ((fn (lambda (u) false)) true)) one) ((mul fn) (fact (decr fn)))))

(define (to-int-repr fn)
  ((fn (lambda (x)
         (+ x 1))) 0))

(define (from-int-repr n)
  (if (equal? n 0)
      zero
      (lambda (f)
        (lambda (x)
          (f (((from-int-repr (- n 1)) f) x))))))

(define (fact-int n)
  (to-int-repr (fact (from-int-repr n))))