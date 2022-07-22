#lang racket
(define (ZERO p)
   (lambda (x) x))

(define (ONE p)
  (lambda (x) (p x)))

(define (TWO p)
  (lambda (x) (p(p x)))
)

(define (THREE p)
  (lambda (x) (p (p (p x))))
)

(define (FIVE p)
  (lambda (x) (p (p (p (p (p x))))))
)

(define (FIFTEEN p)
  (lambda (x) (p(p(p(p(p(p(p(p(p(p(p(p(p(p(p x))))))))))))))))
)

(define (HUNDRED p)
  (lambda (x) (p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(p x)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
)





(define (to-integer proc)
  ((proc (lambda (n) (+ 1 n))) 0)
  )



(to-integer ZERO)
(to-integer ONE)
(to-integer TWO)
(to-integer THREE)
(to-integer FIVE)
(to-integer FIFTEEN)
(to-integer HUNDRED)

#|
(define (gen-int n)
  (define (gen-iter m)
    (if (= m 1)
        "(p x)"
        (string-append "(p" (gen-iter (- m 1)) ")")))
 (string-append "(lambda (x) "
                (gen-iter n)
                ")"))

(gen-int 15)
(gen-int 100)
|#

(define (TRUE x)
  (lambda (y) x)
)
(define (FALSE x)
  (lambda (y) y)
)

(define (to-boolean proc)
  ((proc true) false)
)

(to-boolean TRUE)
(to-boolean FALSE)


(define (_if proc x y)
  ((proc x) y)
 )

(_if TRUE 1 2)

(define (is-zero? proc)
  ((proc (lambda (x) FALSE)) TRUE)
 )

(to-boolean (is-zero? ZERO))


(define (PAIR x)
  (lambda (y) (lambda (f) ((f x) y)))
)

(define (LEFT p)
  (p (lambda (x) (lambda (y) x)))
 )
(define (RIGHT p)
  (p (lambda (x) (lambda (y) y)))
 )

(define pair-test ((PAIR THREE) FIVE))
(to-integer (LEFT pair-test))
(to-integer (RIGHT pair-test))