;; Gives the last atom of a list.
(define (last-element l)
  (if (null? (cdr l))
      (car l)
      (last-element (cdr l))))

;; Makes the next line of the pascal triangle
;; (without the 1's at the extremes) given the previous one.
(define (make-next l)
  (if (null? (cdr l))
      '()
      (cons (+ (car l)
               (cadr l))
            (make-next (cdr l)))))

;; Adds 1 to the front and end of the list.
(define (append1 l)
  (append (cons 1 l)
          (cons 1 '())))

;; Gives the pascal's triangle in reverse order.
(define (rpascal n)
  (cond
    ((= n 0) '((1)))
    ((= n 1) '((1 1) (1)))
    (else (cons (append1
                  (make-next
                    (car (rpascal (- n 1)))))
                (rpascal (- n 1))))))

;; Gives the list l without the last element.
(define (listwithoutlast l)
  (if (null? (cdr l))
      '()
      (cons (car l)
            (listwithoutlast (cdr l)))))

;; Reverses the list.
(define (reverse l)
  (if (null? l)
      '()
      (cons (last-element l)
            (reverse (listwithoutlast l)))))

;; Gives the pascal's triangle.
(define (pascal n)
  (reverse (rpascal n)))
