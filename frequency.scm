;; gives the number of times n appears in the list L
(define (frequency n L)
  (define (freq-rec count ls)
    (if (null? ls)
        count
        (if (= n (car ls))
            (freq-rec (+ count 1) (cdr ls))
            (freq-rec count (cdr ls)))))
  (freq-rec 0 L))
