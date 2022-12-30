#lang racket
;是否为原子值
(define atom?
  (lambda (x) (and (not (pair? x)) (not (null? x)))))
;是否为一维集合
(define lat?
  (lambda (l) (cond
                ((null? l) #t)
                ((atom? (car l)) (lat? (cdr l)))
                (else #f))))
;是否为集合内元素
(define member?
  (lambda (a lat)
    (cond
      ((null? lat)#f)
      ((eq? (car lat) a) #t)
      (else (member? a (cdr lat))))))
;删除第一个值
(define rember
  (lambda (a lat)
    (cond ((null? lat) lat)
          ((eq? a (car lat))(cdr lat))
          (else (cons (car lat) (rember a (cdr lat)))))))
;删除所有相同值
(define delete
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? a (car lat)) (delete a (cdr lat)))
      (else (cons (car lat) (delete a (cdr lat)))))))
;元素头值
(define firsts
  (lambda (lat)
    (cond ((null? lat) lat)
          ((pair? (car lat))(cons (car (car lat)) (firsts (cdr lat))))
          (else (cons (car lat) (firsts (cdr lat)))))
    ))
;原值右插入
(define insertR
  (lambda (new old list)
    (cond ((null? list) list)
          ((eq? old (car list)) (cons (car list) (cons new (cdr list))))
          (else (cons (car list) (insertR new old (cdr list)))))))
;原值左插入
(define insertL
  (lambda (new old list)
    (cond ((null? list)list)
          ((eq? old (car list))(cons new list))
          (else (cons (car list) (insertL new old (cdr list)))))))
;
(define subst
  (lambda (new old list)
    (cond ((null? list) list)
          ((eq? old (car list))(cons new (cdr list)))
          (else (cons (car list) (subst new old (cdr list)))))))
;元素累加
(define addtup
  (lambda (lat)
    (cond ((null? lat)0)
          (else (add (car lat) (addtup (cdr lat)))))))


;数字加1
(define add1
  (lambda (n) (+ n 1)))
;数字减1
(define sub1
  (lambda (n) (- n 1)))
;为0判断
(define zero?
  (lambda (n) (eq? n 0)))
;两数相加
(define add
  (lambda (m n)
    (cond ((zero? n) m)
          (else (add (add1 m) (sub1 n))))))
;两数相减
(define sub
  (lambda (m n)
    (cond ((zero? n) m)
          (else (sub (sub1 m) (sub1 n))))))

;乘法
(define multiply
  (lambda (m n)
    (cond ((zero? n) 0)
          ((eq? n 1) m)
          (else (+ m (multiply m (sub1 n)))))))
;小于
(define lt?
  (lambda (m n)
    (cond ((zero? n) #f)
          ((zero? m) #t)
          (else (lt? (sub1 m) (sub1 n))))))
;小于且等于
(define le?
  (lambda (m n)
    (cond ((and (zero? m)(zero? n)) #t)
          ((zero? n) #f)
          ((zero? m) #t)
          (else (le? (sub1 m) (sub1 n))))))


;向量加法
(define tup+
  (lambda (list1 list2)
    (if (or (null? list1) (null? list2))
        '()
        (cons (add (car list1) (car list2)) (tup+ (cdr list1) (cdr list2))))))
 



;(cdr (list 'a 'b 'c))

;(cdr (list 1 2 3 4))

;(cons 1 (list 2 3 4))

;(cons (list 1 2) (list 3 4 5))

;(cons (list 'a 'b 'c) 'b)

;(cons 1 (cons 2 (cons 3 null)))

;(null? (list))

;(null? "abc")

;(eq? (cdr (cons 1 2)) 2)

;(lat? (list 1 2 3 (cons 1 2)))

;(member? 3 (list 1 2 3))
;(rember 4 (list 1 2 3))
;(rember 1 (list 1 1 1))

(rember 2 (list 1 2 3 3 2 1))
(delete 2 (list 1 2 3 3 2 1))

(zero? 1)
(add 4 5)
(display "➕")
(firsts (list 1 2 (list 3 4) (list (list 5 6) 7)))

(insertR 3 2 (list 1 2 4 5))

(insertL 3 2 (list 1 2 4 5))

(subst 3 2 (list 1 2 4 5))

(addtup '(1 2 3 4))


(define lat '(1 2 3 4))
(define list-set
  (lambda (list pos new-value iter)
    (define list-set!-iter
      (lambda (list pos new-value iter)
        (cond ((= iter pos) (cons new-value (cdr lat)))
              (else (cons (car lat) (list-set!-iter (cdr lat) pos new-value (+ iter 1)))))))
    (list-set!-iter list pos new-value 0)))



(list-set list 0 10 0)
