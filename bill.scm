(define fourth-element
  (lambda (l)
    (car (cdddr (car l)))))

(define sum-of-fourth-elements
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (+ (fourth-element l)
               (sum-of-fourth-elements (cdr l)))))))

;; gives the list containing
;; 1) number of elements in the list
;; 2) sum of the fourth elements of the sublists of the list l
(define total
  (lambda (l)
    (cons (length l)
          (cons (sum-of-fourth-elements l)
                '()))))

;; gives the list containing
;; 1) item-name
;; 2) kg of items
;; 3) price per kg
;; 4) total price paid
(define tobepurchased
  (lambda (a c)
    (cons (caar a)
          (cons (cadar a)
                (cons (cadar c)
                      (cons (* (cadar a)
                               (cadar c))
                            '()))))))

;; first element a, b, c = item-name
;; second-element a = no-of-kgs to purchase
;; second-element b = maximum price to be spent on 1 kg of item
;; second-element c = actual price per kg
;; if b of some item is not listed, purchase it irrespective of the price.
;;
;; bill containing lists of
;; 1) item name
;; 2) kg of item
;; 3) price per kg
;; 4) price paid
(define bill2
  (lambda (a b c)
    (cond
      ((null? a) '())
      ((or (null? b)
           (not (eq? (caar a) (caar b))))
       (cons (tobepurchased a c)
             (bill2 (cdr a) b (cdr c))))
      ((<= (cadar c) (cadar b))
       (cons (tobepurchased a c)
             (bill2 (cdr a) (cdr b) (cdr c))))
      (else (bill2 (cdr a) (cdr b) (cdr c))))))

;; bill2 +
;; 1) total items purchased
;; 2) total cost
(define bill
  (lambda (a b c)
    (append (bill2 a b c)
            (cons (total (bill2 a b c))
                  '()))))
