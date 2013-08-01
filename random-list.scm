; return true if the number "n" exists in the list "l", otherwise false
(define (exists? n l)
  (if (null? l)
      #f
      (if (= n (car l))
          #t
          (exists? n (cdr l)))))

; generate a random number belonging to [1,n]
(define (random-number n)
  (+ (random n)
     1))

; adds "n - count" number of random numbers (<n) to the list l
(define (fill-random n l count)
  (define rand (random-number n))
  (if (= n count)
      l
      (if (exists? rand l)
          (fill-random n l count)
          (fill-random n (cons rand l) (+ count 1)))))

; dummy funciton to call the above function with a null list & 0 as the 2nd & 3rd arguments
(define (random-list n)
  (fill-random n '() 0))
