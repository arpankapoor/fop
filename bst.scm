;; Constructors and selectors for a Binary Search Tree
(define (make-bst value ls rs)
  (list value ls rs))

(define (bst-v bst) (car bst))
(define (bst-ls bst) (cadr bst))
(define (bst-rs bst) (caddr bst))

(define make-empty-bst (list))
(define (empty-bst? bst) (null? bst))
(define (leaf? bst)
  (and (empty-bst? (bst-ls bst))
       (empty-bst? (bst-rs bst))))

;; Insert a value v into the BST
(define (insert v bst)
  (cond ((empty-bst? bst)
         (make-bst v
                   make-empty-bst
                   make-empty-bst))
        ((< v (bst-v bst))
         (make-bst (bst-v bst)
                   (insert v (bst-ls bst))
                   (bst-rs bst)))
        ((> v (bst-v bst))
         (make-bst (bst-v bst)
                   (bst-ls bst)
                   (insert v (bst-rs bst))))
        (else bst)))

;; Convert a list to a BST, assuming the first element of the list to be the
;; root of the BST.
(define (lst->bst lst)
  (define (convert ls bst)
    (if (null? ls)
        bst
        (convert (cdr ls)
                 (insert (car ls)
                         bst))))
  (convert lst (list)))

;; Convert a BST to sorted list.
(define (bst->lst bst)
  (if (empty-bst? bst)
      (list)
      (append (bst->lst (bst-ls bst))
              (list (bst-v bst))
              (bst->lst (bst-rs bst)))))

;; Leftmost child of bst
;; Precondition: BST should not be empty
(define (leftmost bst)
  (if (empty-bst? (bst-ls bst))
      (bst-v bst)
      (leftmost (bst-ls bst))))

;; Delete any node from a BST
(define (delete v bst)
  (cond ((empty-bst? bst) bst)
        ((< v (bst-v bst))
         (make-bst (bst-v bst)
                   (delete v (bst-ls bst))
                   (bst-rs bst)))
        ((> v (bst-v bst))
         (make-bst (bst-v bst)
                   (bst-ls bst)
                   (delete v (bst-rs bst))))
        ((= v (bst-v bst))
         (delete-root bst))))

;; Delete the root of the BST
(define (delete-root bst)
  (cond ((leaf? bst) make-empty-bst)
        ((empty-bst? (bst-ls bst))
         (bst-rs bst))
        ((empty-bst? (bst-rs bst))
         (bst-ls bst))
        (else (make-bst (leftmost (bst-rs bst))
                        (bst-ls bst)
                        (delete (leftmost (bst-rs bst)) (bst-rs bst))))))

;; Check if v is a member of the BST
(define (member? v bst)
  (cond ((empty-bst? bst) #f)
        ((< v (bst-v bst))
         (member? v (bst-ls bst)))
        ((> v (bst-v bst))
         (member? v (bst-rs bst)))
        (else #t)))

;; Return the number of items in the BST
(define (bst-size bst)
  (if (empty-bst? bst)
      0
      (+ 1
         (bst-size (bst-ls bst))
         (bst-size (bst-rs bst)))))

;; Return the height of the tree.
;; Height of empty bst = -1
(define (bst-height bst)
  (if (empty-bst? bst)
      -1
      (+ 1
         (max (bst-height (bst-ls bst))
              (bst-height (bst-rs bst))))))

(define (shortest-path-to-leaf bst)
  (if (empty-bst? bst)
      -1
      (+ 1
         (min (shortest-path-to-leaf (bst-ls bst))
              (shortest-path-to-leaf (bst-rs bst))))))

(define (bst-balanced? bst)
  (if (empty-bst? bst)
      #t
      (and (= (bst-height (bst-ls bst))
              (bst-height (bst-rs bst)))
           (bst-balanced? (bst-ls bst))
           (bst-balanced? (bst-rs bst)))))
