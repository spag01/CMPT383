#lang racket

(define (len2 lst)
  (if (empty? lst) 0 ( + 1 ( len2(rest lst))))
  )

(define (index-of x lst )
  (if(null? lst)
     -1
     (if( equal? x (first lst))
    0
    (let([ans (index-of x (rest lst))])
             (if(= ans -1)
                -1
                (+ 1 ans))))
    )
  )

(define (contains x lst)
  (cond [(empty? lst)           #f]
        [(equal? x (first lst)) #t]
        [else (contains x (rest lst))]
))

(define (my-subset? lst1 lst2)
  (if(null? lst1)
     #t
     (if(contains (first lst1) lst2)
        (my-subset? (rest lst1) lst2)
        #f)
     )
  )

(define (same-set? lst1 lst2)
  (and(my-subset? lst1 lst2)
      (my-subset? lst2 lst1))
  )


(define (pairs lst1 lst2)
  (cond[(or(null? lst1) (null? lst2))  '()]
       [else (cons(list (first lst1) (first lst2))
             (pairs (rest lst1) (rest lst2)))]
       )
  )

(define (my-remove-duplicates lst)
  (cond[(empty? lst) '() ]
       [(contains (first lst) (rest lst))
        (my-remove-duplicates (rest lst))]
       [else
        (cons (first lst) (my-remove-duplicates (rest lst)))]
       )
  )