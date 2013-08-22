(define (min lat)
  (define (min-iter m l)
    (cond 
      ((null? l) m)
      ((< (car l) m)
       (min-iter (car l) (cdr l)))
      (else (min-iter m (cdr l)))))
  (min-iter (car lat) lat))

(define (sort-a lat)
  (cond
    ((null? lat) '())
    (else (cons (min lat)
                (sort-a (rember (min lat) lat))))))

(define (rember n lat)
  (cond
    ((null? lat) '())
    ((= n (car lat)) (cdr lat))
    (else (cons (car lat)
                (rember n (cdr lat))))))
