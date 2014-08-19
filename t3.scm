;; Constructors and selectors for time
(define (make-time hr min) (cons hr (list min)))
(define (get-hr time) (car time))
(define (get-min time) (cadr time))

;; Constructors and selectors for trip record
(define (make-trip-rec regno start-time reach-time)
  (cons regno (cons start-time (list reach-time))))
(define (get-regno trip-rec) (car trip-rec))
(define (get-start-time trip-rec) (cadr trip-rec))
(define (get-reach-time trip-rec) (caddr trip-rec))

;; Constructors and selectors for making a route-log
(define (make-route-log . trip-recs) trip-recs)
(define (get-first-trip-rec route-log) (car route-log))
(define (rest-of route-log) (cdr route-log))

;; Returns the time-taken by a trip in hours
(define (time-taken trip)
  (+ (- (get-hr (get-reach-time trip))
        (get-hr (get-start-time trip)))
     (/ (remainder (- (+ (get-min (get-reach-time trip)) 60)
                      (get-min (get-start-time trip)))
                   60)
        60)))

;; Checks if time taken by trip1 is lesser than trip2
(define (faster? trip1 trip2)
  (< (time-taken trip1) (time-taken trip2)))

;; Returns the trip-record of the fastest trip
(define (fastest route-log)
  (if (null? (rest-of route-log))
    (get-first-trip-rec route-log)
    (if (faster? (get-first-trip-rec route-log)
                 (fastest (rest-of route-log)))
      (get-first-trip-rec route-log)
      (fastest (rest-of route-log)))))

;; Gives the registration number of the fastest trip-record
(define (main route-log)
  (get-regno (fastest route-log)))
