;; Gives the minimum value in a list.
(define (min lat)
  (define (min-iter m l)
    (cond 
      ((null? l) m)
      ((< (car l) m)
       (min-iter (car l) (cdr l)))
      (else (min-iter m (cdr l)))))
  (min-iter (car lat) lat))

;; Recursive and better algorithm.
;; suggestion by WAGHELA.
(define (min lat)
  (if (null? (cdr lat))
      (car lat)
      (if (> (car lat) (cadr lat))
          (min (cdr lat))
          (min (rember (cadr lat) lat)))))

;; Sort in ascending order.
(define (sort-a lat)
  (cond
    ((null? lat) '())
    (else (cons (min lat)
                (sort-a (rember (min lat) lat))))))

;; Remove a member from the list.
(define (rember n lat)
  (cond
    ((null? lat) '())
    ((= n (car lat)) (cdr lat))
    (else (cons (car lat)
                (rember n (cdr lat))))))
