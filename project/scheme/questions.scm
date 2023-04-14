(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement

(define (zip pairs)
  (cons (map car pairs) (cons (map cadr pairs) nil))
)


;; Problem 15
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 15
  (define
    (enumerate-from n s)
      (cond
        ((null? s) nil)
        (else (cons (cons n (cons (car s) nil)) (enumerate-from (+ n 1) (cdr s))))
      )
  )
  (enumerate-from 0 s)
)
  ; END PROBLEM 15

;; Problem 16

;; Merge two lists LIST1 and LIST2 according to COMP and return
;; the merged lists.
(define (merge comp list1 list2)
  ; BEGIN PROBLEM 16
  (cond
    ((eq? comp <) 
      (cond
        ((null? list1) list2)
        ((null? list2) list1)
        ((< (car list1) (car list2)) (cons (car list1) (merge < (cdr list1) list2)))
        ((> (car list1) (car list2)) (cons (car list2) (merge < list1 (cdr list2))))
      )
    )
    ((eq? comp >) 
      (cond
        ((null? list1) list2)
        ((null? list2) list1)
        ((< (car list1) (car list2)) (cons (car list2) (merge > list1 (cdr list2))))
        ((> (car list1) (car list2)) (cons (car list1) (merge > (cdr list1) list2)))
      ) 
    )
  )
)
  ; END PROBLEM 16


;; Problem 17

;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond ((atom? expr)
         ; BEGIN PROBLEM 17
         expr
         ; END PROBLEM 17
         )
        ((quoted? expr)
         ; BEGIN PROBLEM 17
         expr
         ; END PROBLEM 17
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 17
           (cons form (cons params (map let-to-lambda body)))
           ; END PROBLEM 17
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 17
           (cons (cons 'lambda (cons (car (zip (let-to-lambda values))) (let-to-lambda body))) (car (cdr (zip (let-to-lambda values)))))
           ; END PROBLEM 17
           ))
        (else
         ; BEGIN PROBLEM 17
         (map let-to-lambda expr)
         ; END PROBLEM 17
         )))

