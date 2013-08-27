;; gives the first half of a list
(define (first-half l)
  (define (half lst n)
    (if (even? n)
        (if (= (length lst) (/ n 2))
            '()
            (cons (car lst)
                  (half (cdr lst) n)))
        (if (= (length lst) (/ (- n 1) 2))
            '()
            (cons (car lst)
                  (half (cdr lst) n)))))
  (half l (length l)))

;; give the second half of a list
(define (second-half l)
  (define (half lst n)
    (if (even? n)
        (if (= (length lst) (/ n 2))
            lst
            (half (cdr lst) n))
        (if (= (length lst) (/ (- n 1) 2))
            lst
            (half (cdr lst) n))))
  (half l (length l)))

;; Merge 2 sorted lists into a single sorted one.
(define merge
  (lambda (a b)
    (cond
      ((and (null? a) (null? b))
       '())
      ((null? a) b)
      ((null? b) a)
      ((< (car a) (car b))
       (cons (car a)
             (merge (cdr a) b)))
      (else (cons (car b)
                  (merge a (cdr b)))))))

;; sort a list in increasing order
(define sort
  (lambda (l)
    (if (null? (cdr l))
        l
        (merge (sort (first-half l))
               (sort (second-half l))))))
