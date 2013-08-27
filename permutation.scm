;; insert new to the right of the first occurence
;; of old in the list
(define (insertR new old l)
  (cond
    ((null? l) '())
    ((= (car l) old)
     (cons old
           (cons new
                 (cdr l))))
    (else (cons (car l)
                (insertR new old (cdr l))))))

;; Outputs the permutations of the number n with
;; the elements of the list.
;;
;; Iterate over the list such that n is inserted to
;; the right of each of the elements of the list.
;;
;; Add the case when n is inserted to the beginning
;; of the list.
(define (perm-list n l)
  (define (iter-over-list iter-l)
    (if (null? iter-l)
        (cons (cons n l) '())
        (cons (insertR n (car iter-l) l)
              (iter-over-list (cdr iter-l)))))
  (iter-over-list l))

;; Gives the permutaions of the number n with
;; each of the sublists of lol (list-of-lists).
;;
;; just append the permutations of n with all
;; the different lists in lol.
(define (perm-many-lists n lol)
  (if (null? lol)
      '()
      (append (perm-list n (car lol))
              (perm-many-lists n (cdr lol)))))

;; Permutations of the given list
;; 
;; Iterate over the list until it has just 1
;; element, in which case it'll have only 1 permutation
(define (permutation l)
  (if (null? (cdr l))
      (cons (cons (car l)
                  '())
            '())
      (remove-dups (perm-many-lists (car l)
                       (permutation (cdr l))))))

;; PART II

;; remove any duplicates present in the list
(define (remove-dups l)
  (cond
    ((null? l) '())
    (else (cons (car l)
                (remove-dups (remove (car l)
                                     l))))))

;; remove the list s from the list-of-lists l
(define (remove s l)
  (cond
    ((null? l) '())
    ((eqlist? s (car l))
     (remove s (cdr l)))
    (else (cons (car l)
                (remove s (cdr l))))))

;; check if two lists (containing only numbers)
;; are equal or not
(define (eqlist? l1 l2)
  (cond
    ((and (null? l1) (null? l2))
     #t)
    ((= (car l1) (car l2))
     (eqlist? (cdr l1) (cdr l2)))
    (else #f)))
