;; determine the smallest sum
(define (smallest-sum lol)
  (if (null? lol)
      'error
      (minimum (list-of-sums lol))))

;; get a list of the sums of each of the sublists
;; of a list
(define (list-of-sums lol)
  (if (null? lol)
      '()
      (cons (sum (car lol))
            (list-of-sums (cdr lol)))))

;; sum of the elements of a list
(define (sum l)
  (if (null? l)
      0
      (+ (car l)
         (sum (cdr l)))))

;; gives the minimum of a list
(define (minimum l)
  (if (null? (cdr l))
      (car l)
      (if (> (car l) (cadr l))
          (minimum (cdr l))
          (minimum (cons (car l)
                         (cddr l))))))
