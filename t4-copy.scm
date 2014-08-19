;; Constructors and selectors for a binary tree
(define (make-bt root ls rs) (list root ls rs))

(define (get-root bt) (car bt))
(define (get-ls bt) (cadr bt))
(define (get-rs bt) (caddr bt))
(define (leaf? bt)
  (and (null? (get-ls bt))
       (null? (get-rs bt))))

;; Return a binary tree whose root is root
;; Sample I/P: (search-root (list 1 (list "A" (list) (list)) (list)) "A")
;; O/P: ("A" () ())
(define (search-root bt root)
  (cond ((null? bt) bt)
        ((equal? root (get-root bt)) bt)
        ((null? (search-root (get-ls bt) root))
         (search-root (get-rs bt) root))
        (else (search-root (get-ls bt) root))))

;; Convert a binary tree into a list
;; Sample I/P: (bt->lst (list 1 (list "A" (list) (list)) (list)))
;; O/P: ("A" 1)
(define (bt->lst bt)
  (if (null? bt)
      (list)
      (append (bt->lst (get-ls bt))
              (list (get-root bt))
              (bt->lst (get-rs bt)))))

;; Return all the descendents of a person with a given name and family
;; Sample I/P: (main1 (list "A" (list "B" (list "C" (list) (list)) (list)) (list)) "A")
;; O/P: ("C" "B")
(define (main1 family name)
  (append (bt->lst (get-ls (search-root family name)))
          (bt->lst (get-rs (search-root family name)))))

;; Return a list of all the leaves present in the binary tree
;; Sample I/P: (leaves (list 1 (list "A" (list) (list)) (list "B" (list) (list))))
;; O/P: ("A" "B")
(define (leaves bt)
  (cond ((null? bt) bt)
        ((leaf? bt) (list (get-root bt)))
        (else (append (leaves (get-ls bt))
                      (leaves (get-rs bt))))))

;; Return a list of all descendant of a person with no children
;; Sample I/P: (main2 (list "A" (list "B" (list "C" (list) (list)) (list)) (list)) "A")
(define (main2 family name)
  (leaves (search-root family name)))

;; Check if name is a member of the given family
;; Sample I/P: (member? (list "A" (list) (list)) "A")
;; O/P: #t
(define (member? family name)
  (not (null? (search-root family name))))

;; Return the nearest common ancestor of name1 and name2 in the family 
;; Sample I/P:
;; O/P: "A"
(define (main3 family name1 name2)
  (cond ((equal? name1 (get-root family)) name1)    ; Case where one of the persons
        ((equal? name2 (get-root family)) name2)    ; is an ancestor of the other.
        ((or (and (member? (get-ls family) name1)   ; the persons are members of
                  (member? (get-rs family) name2))  ; different subtrees.
             (and (member? (get-ls family) name2)
                  (member? (get-rs family) name1)))
         (get-root family))
        ((member? (get-ls family) name1)
         (main3 (get-ls family) name1 name2))
        (else
          (main3 (get-rs family) name1 name2))))
(main3 (make-bt "A"
                 (make-bt "B"
                          (make-bt "C"
                                   (list)
                                   (list))
                          (make-bt "D"
                                   (list)
                                   (list)))
                 (make-bt "E"
                         (make-bt "F"
                                   (list)
                                  (list))
                         (make-bt "G"
                                   (list)
                                   (list))))
        "B"
        "D")
