#lang racket

(define (add-vote candidate votes_map)
  (hash-set! votes_map candidate (+ 1 (hash-ref votes_map candidate 0)))
  votes_map
  )

(define (count_votes file)
  (read-next-line-iter file (make-hash))
  )                      

(define (read-next-line-iter file votes_map)
  (let ((line (read-line file 'any)))
    (if (eof-object? line)
        votes_map
        (let ([m (foldl (lambda (c m) (add-vote c m)) votes_map (remove-duplicates (string-split line)))])
          (read-next-line-iter file m)
          )
      
        )
    )
  )


(display "What is the name of the ballot file?\n")
(define f_name (read-line (current-input-port) 'any))

(define total_votes (call-with-input-file f_name count_votes))
(define empty_ballots_number (hash-ref total_votes "none" 0))
(hash-remove! total_votes "none")

;(display total_votes)
(define candidates_set (set-remove  (list->set (hash-keys total_votes)) "none"))

;(display candidates_set)

(define (count_full file)
  (check_line_is_full file 0 0)
  )                      

(define (check_line_is_full file n t)
  (let ((line (read-line file 'any)))
    (if (eof-object? line)
        (cons n t)
        (let ([_n (if (equal? candidates_set (list->set (string-split line)))
                      (+ 1 n)
                      n
                      )])
          (check_line_is_full file _n (+ 1 t)) 
          )
        )
    )
  )

(define ballos_stats (call-with-input-file f_name count_full))
(define full_ballots_number (car ballos_stats))
(define total_ballots_number (cdr ballos_stats))

(newline)
(display "Total # of ballots: ")
(display total_ballots_number)
(newline)
(newline)

(define sorted (sort (hash->list total_votes) > #:key (lambda (x) (cdr x))))
(for-each (lambda (x) (display (~a (car x) ": " (cdr x) "\n")) ) sorted)
(newline)
(display (~a "empty: " empty_ballots_number "\n"))
(display (~a "full: " full_ballots_number "\n"))
