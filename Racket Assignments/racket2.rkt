#lang racket

;;Question 1
(define (is-digit? c)
  ( if (char? c)
       (and (<= (char->integer c) 57)
                       (>= (char->integer c) 48))
       #f)
  )
       
;;Question 2
(define (is-letter? c)
  (if (char? c)
      (or
       (and(<= (char->integer c) 90) (>= (char->integer c) 65))
       (and(<= (char->integer c) 122) (>= (char->integer c) 97))
       )
      #f)
  )

;;Question 3 
(define (convert-symbol sym) ;; helper function1
  (string->list              ;; convert a symbol into list of racket char values 
   (symbol->string sym)
   )
  )

(define (contain-symbol sym) ;;helper function2 
  (match sym                 ;;chech if the symbol is either of 'not, 'or, 'and, 'f, 't 
    ['f   #f]
    ['t   #f]
    ['not #f]
    ['and #f]
    ['or  #f]
    [_    #t])
  )
(define (is-vble? x)
(cond [(and (symbol? x)(equal?(contain-symbol x) #t))  
      (if (is-letter? (first (convert-symbol x)))
          (if (equal? (length(convert-symbol x))
               (length (append(filter is-letter?(convert-symbol x)) (filter is-digit? (convert-symbol x)))))
               #t
               #f)
          #f
          )
      ]
      
      [else #f ]
      )
  )


;;Question 5
#|(define ( is-expr? e)
  (if (member e '(t f))
      #t
  (match e
    ['(is-vble? e)      #t]
    ['( not , a)     (is-expr? a)]
    ['(,e1 and ,e2)  (and (is-expr? e1) (is-expr? e2))]
    ['(,e1 or ,e2)   (and (is-expr? e1) (is-expr? e2))]
    ['(,e1 -> ,e2)   (and (is-expr? e1) (is-expr?e2))]
    [_    #f]
    ))
  )|#










                      
      
  