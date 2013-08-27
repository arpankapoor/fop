;; gives the number of times n appears in the list L
(define (frequency n L)
  (define (freq-rec count ls)
    (if (null? ls)
        count
        (if (= n (car ls))
            (freq-rec (+ count 1) (cdr ls))
            (freq-rec count (cdr ls)))))
  (freq-rec 0 L))

;; same function using deep recursion.
(define (frequency1 n L)
  (cond
    ((null? L) 0)
    ((= n (car L))
     (+ 1 (frequency n (cdr L))))
    (else (frequency n (cdr L)))))
