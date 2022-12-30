#lang racket
;是否是list成员
(define is-member?
  (lambda (list item)
    (if (null? list) #f (if (eq? item (car list)) #t (is-member? (cdr list) item)))))
;日期定义
(define date
  (lambda (year month day)
    (lambda (proc)
      (proc year month day))))
;获取年份
(define year
  (lambda (date) (date (lambda (y m d) y) )))

;获取月份
(define month
  (lambda (date) (date (lambda (y m d) m) )))

;获取所在月份的天数
(define day
  (lambda (date) (date (lambda (y m d) d) )))
;获取星期数(1-7:周一至周日)#;TODO
(define week
  (lambda (d)
    (let ([diff (remainder (+ 4 (remainder (diff-days (date 1970 1 1) d)7))7)])
      (if (= diff 0) 7 diff))))
;星期加1天
(define incr-week
  (lambda (w)
    (if (= w 7) 1 (+ w 1))))

;日期加一天
(define add-day
  (lambda (d)
    (let ([y (year d)][m (month d)][d (day d)])
      (cond ((<= (+ 1 d) (month-day-count y m))(date y m (+ d 1)))
            ((<= (+ 1 m) 12)(date y (+ m 1) 1))
            (else (date (+ 1 y) 1 1))))))

;日期加N天
(define add-days
  (lambda (d n)
    (if (<= n 0) d (add-days (add-day d) (- n 1)))))


;时间大小比较
(define date-before?
  (lambda (date1 date2)
    (or (<  (year date1) (year date2)) (<  (month date1) (month date2)) (<  (day date1) (day date2)))))
;时间相等比较
(define date-equal?
  (lambda (date1 date2)
    (and (=  (year date1) (year date2)) (=  (month date1) (month date2)) (=  (day date1) (day date2)))))
;日期转字符串
(define date->string
  (lambda (d) (d (lambda (y m d)
                   (string-append (number->string y) "-" (number->string m) "-" (number->string d))))))




;闰年判断
(define is-leap-year?
  (lambda (year)
    (if (eq? 0 (remainder year 100))
        (eq? 0 (remainder year 400))
        (eq? 0 (remainder year 4)))
    ))

;获取年份全年天数
(define year-day-count
  (lambda (year)
    (if (is-leap-year? year) 366 365)
    ))
;获取月份全部天数
(define month-day-count
  (lambda (year month)
    (cond ((eq? 2 month) (if (is-leap-year? year) 29 28))
          ((is-member? (list 1 3 5 7 8 10 12) month) 31)
          (else 30))
    ))
;两个日期相隔的天数
(define diff-days
  (lambda (sdate edate)
    (if (date-before? sdate edate) (+ 1 (diff-days (add-day sdate) edate)) 0)))


;打印日历
(define (print-calendar dt)
  (define (get-day n w month-days)
    (cond ((= n month-days) (number->string n))
          ((<= n 0) (string-append "   " (get-day (+ 1 n) w month-days)))
          (else (string-append (if (< n 10) " " "")(number->string n)
                       (if (= 7 w) "\n" " ")
                       (get-day (+ 1 n) (incr-week w) month-days)))))
  (let* ([y (year dt)]
         [m (month dt)]
         [d (day dt)]
         [w (week (date y m 1) )]
         [n (- 2 w)]
         [end-day (month-day-count y m)])
    
    (string-append
     "====================\n"
     (number->string y) "." (number->string m) "\n"
     "====================\n"
     "Mo Tu We Th Fr Sa Su\n"
     "====================\n"
     (get-day n w end-day)
     "\n"
     "====================\n")))
  
;(year (date 2022 12 28))
;(month (date 2022 12 28))
;(day (date 2022 12 28))
;(week (date 2022 12 28))

;(date->string (add-day (date 2022 12 28)))

;(date->string (add-days (date 1970 1 1) 20000))

;(date->string (add-day (date 2022 12 31)))
;(date->string (add-day (date 2022 11 30)))

;(date->string (date 2022 12 28))


;(date-before? (date 2022 12 29) (date 2022 12 28))



;(diff-days (date 1970 1 1) (date 2024 10 4))
(display (print-calendar (date 2022 12 29)))

(display (print-calendar (date 2023 1 1)))
(display (print-calendar (date 2023 4 1)))
;(print-calendar (date 2022 12 29))



 