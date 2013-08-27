;; main function 
(define (main lol)
  (if (dates-valid? lol)
      (eldest lol)
      'invalidinput))

;; gives the year of birth of a particular person
(define (year l)
  (car (cdr (cdr (car (cdr l))))))

;; month of birth of a person
(define (month l)
  (car (cdr (car (cdr l)))))

;; day of birth of a person
(define (date l)
  (car (car (cdr l))))

;; checks if all the supplied dates are valid
(define (dates-valid? lol)
  (if (null? lol)
      #t
      (if (> (year (car lol)) 0)
          (if (and (> (month (car lol)) 0)
                   (< (month (car lol)) 13))
              (and (> (date (car lol)) 0)
                   (cond
                     ((= (month (car lol)) 2)
                      (< (date (car lol)) 29))
                     ((or (= (month (car lol)) 4)
                          (= (month (car lol)) 6)
                          (= (month (car lol)) 9)
                          (= (month (car lol)) 11))
                      (< (date (car lol)) 31))
                     (else (< (date (car lol)) 32)))
                   (dates-valid? (cdr lol)))
              #f)
          #f)))

;; gives the eldest person's name
(define (eldest lol)
  (cond
    ((= (length (least-yrs lol))
        1)
     (caar (least-yrs lol)))
    ((= (length (least-mnths
                  (least-yrs lol)))
        1)
     (caar (least-mnths
            (least-yrs lol))))
    ((= (length (least-dates
                  (least-mnths
                    (least-yrs lol))))
        1)
     (caar (least-dates
            (least-mnths
              (least-yrs lol)))))
    (else (firsts (least-dates
                    (least-mnths
                      (least-yrs lol)))))))

;; gives the names of people having same dob
(define (firsts lol)
  (if (null? lol)
      '()
      (cons (caar lol)
            (firsts (cdr lol)))))

;; gives a list having info about people with the lowest year of birth
(define (least-yrs lol)
  (define min (min-yr lol))
  (define (iter l)
    (if (null? l)
        '()
        (if (= (year (car l))
               min)
          (cons (car l)
                (iter (cdr l)))
          (iter (cdr l)))))
  (iter lol))

;; gives a list having info about people with the lowest month of birth
(define (least-mnths lol)
  (define min (min-mnth lol))
  (define (iter l)
    (if (null? l)
        '()
        (if (= (month (car l))
               min)
          (cons (car l)
                (iter (cdr l)))
          (iter (cdr l)))))
  (iter lol))

;; gives a list having info about people with the lowest day of birth
(define (least-dates lol)
  (define min (min-date lol))
  (define (iter l)
    (if (null? l)
        '()
        (if (= (date (car l))
               min)
          (cons (car l)
                (iter (cdr l)))
          (iter (cdr l)))))
  (iter lol))

;; finds the lowest year of birth from the list
(define (min-yr lol)
  (if (null? (cdr lol))
      (year (car lol))
      (if (< (year (car lol))
             (min-yr (cdr lol)))
          (year (car lol))
          (min-yr (cdr lol)))))

;; finds the lowest month of birth from the given list
(define (min-mnth lol)
  (if (null? (cdr lol))
      (month (car lol))
      (if (< (month (car lol))
             (min-mnth (cdr lol)))
          (month (car lol))
          (min-mnth (cdr lol)))))

;; finds the lowest day of birth from the given list
(define (min-date lol)
  (if (null? (cdr lol))
      (date (car lol))
      (if (< (date (car lol))
             (min-date (cdr lol)))
          (date (car lol))
          (min-date (cdr lol)))))
