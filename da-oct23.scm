;; In DSS under Running Courses, there are many Courses. For a particular
;; Course (subject, say FOP) we have Course number, Course name, Faculty name,
;; Timetable Slot, and the following details of course participants - Sl. No.
;; (starting from 1), Roll No. (NITC format), Name (first only) and Gender.

;; Constructor and selectors for running courses
(define (make-running-courses . courses) courses)

(define (get-first-course rc) (car rc))
(define (rest-of rc) (cdr rc))
(define (empty? rc) (null? rc))

;; Constructor and selectors for a course
(define (make-course number name facname slot participants)
  (list number name facname slot participants))

(define (get-course-number course) (car course))
(define (get-course-name course) (cadr course))
(define (get-course-facname course) (caddr course))
(define (get-course-slot course) (cadddr course))
(define (get-course-participants course) (car (cddddr course)))

;; Constructor and selectors for participants
(define (make-participant slno rollno name gender)
  (list slno rollno name gender))

(define (get-participant-slno p) (car p))
(define (get-participant-rollno p) (cadr p))
(define (get-participant-name p) (caddr p))
(define (get-participant-gender p) (cadddr p))

;; From DSS Course Area, write the design and implement the functions to answer
;; the following questions.

;; 1) Given two courses  (I/P: two course codes and Running Courses) check
;;    whether they run in the same slot.

;; Get the course with course code cc
(define (search-cc running-courses cc)
  (if (empty? running-courses)
      #f
      (if (= (get-course-number (get-first-course running-courses))
             cc)
          (get-first-course running-courses)
          (search-cc (rest-of running-courses) cc))))

(define (same-slot? cc1 cc2 running-courses)
  (eq? (get-course-slot (search-cc running-courses cc1))
       (get-course-slot (search-cc running-courses cc2))))
      

;; 2) Given a roll number (I/P: roll number and Running Courses) find that
;;    student's all current teachers.

;; Check if a student with roll number rollno is a participant in a course
;; (Given list of course participants and rollno)
(define (participant? rollno participants)
  (if (empty? course)
      #f
      (if (= rollno (get-participant-rollno (car participants)))
          #t
          (participant? rollno (cdr participants)))))

(define (roll-teachers rollno rc)
  (if (empty? rc)
      (list)
      (if (participant? rollno (get-course-participants (get-first-course rc)))
          (cons (get-course-facname (get-first-course rc))
                (roll-teachers rollno (rest-of rc)))
          (roll-teachers rollno (rest-of rc)))))
