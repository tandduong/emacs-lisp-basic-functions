;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

;; Assignment 9 - Tan D. Duong - COSC455
;; Problem 1

(defun dot-product (l1 l2)
  (cond
   ( (null l1) 0)
   ( (+ (* (car l1) (car l2)) (dot-product (cdr l1) (cdr l2))))
   )
  )

;; return 19
(dot-product '(1 2 3) '(3 2 4))


;; Problem 2

(defun member (x l)
  (cond
   ( (null l) () )
   ( (eq (car l) x) t)
   ( t (member x (cdr l)))
   )
  )

(defun subset (l1 l2)
  (cond
   ( (null l1) t)
   ( (member (car l1) l2) (subset (cdr l1) l2))
   ( t nil)
   )
  )

;; true

(subset '(1 2) '(0 3 2 1))

;; false

(subset '(1 2) '(0 3 2 4))


;; Problem 3

(defun insertL (new old l)
  (cond
   ( (null l) l)
   ( (eq (car l) old) (cons new (cons (car l) (cdr l))))
   ( t (cons (car l) (insertR new old (cdr l))))
   )
  )

;; return (1 2 3 4)

(insertL '1 '2 '(2 3 4))

;; Problem 4

(defun substall (new old l)
  (cond
   ( (null l) () )
   ( (eq (car l) old) (cons new (substall new old (cdr l))))
   ( t (cons (car l) (substall new old (cdr l))))
   )
  )

;; Return (1 1 1)

(substall 1 3 '(3 3 3))


;; Problem 5

(defun rember (x l)
  (cond
   ( (null l))
   ( (eq x (car l)) (cdr l))
   ( t (cons (car l) (rember x (cdr l))))
   )
  )

(rember '3 '(1 3 4 5))

(defun rember2 (x l)
  (cond
   ( (null l))
   ( (eq (car l) x) (cons (car l) (rember x (cdr l))))
   ( t (cons (car l) (rember2 x (cdr l))))
   )
  )

;; Return (1 2 3 2 3)

(rember2 '1 '(1 2 3 1 2 3))


;; Problem 6
(defun increment (x)
  (cond
   ( (null x) ())
   (t (+ x 1))
   )
  )
(defun occur (x l)
  (cond
   ( (null l) 0)
   ( (null x) 0)
   ( t (cond
	( (eq (car l) x) (increment (occur x (cdr l))))
	( t (occur x (cdr l)))
	)
       )
   )
  )
(defun occurN (l1 l2)
  (cond
   ((null l1) 0)
   ((null l2) 0)
   (t (+ (occur (car l1) l2)(occurN (cdr l1) l2)))
   )
  )

;; return 2
(occurN '(1) '(3 4 1 1 5 3))


;; Problem 7

(defun pairs (l1 l2)
  (cond
   ( (null l1) l2)
   (t (cons(cons (car l1) (car l2)) (pairs (cdr l1) (cdr l2))))
   )
  )

;; I got it return as pairs but it shows like (1 . a) (2 . b) (3 . c) (4 . d) 
(pairs '(1 2 3 4) '(a b c d))

;; Problem 8

(defun assoc (a l)
  (cond
   ((null l) nil)
   ((eq a (caar l)) (car l))
   (t (cdr (assoc a (cdr l))))
   )
  )

;; Return (2)

(assoc 'y '((x 1) (y 2) (z 3)))

;; Return (90)

(assoc 'v '((f 3) (v 90) (c 81)))
