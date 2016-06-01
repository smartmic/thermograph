#!/usr/bin/guile -s
!#
;;;coding:utf-8
(setlocale LC_ALL "")
(define (reduce fn base-value lis)
  (if (null? lis)
    base-value
    (fn (car lis)
        (reduce fn base-value (cdr lis)))))


(define ws "water")
(define boundary-list '(0 2))

(define p "p")
(define m "m")
(define t "t")
(define in (lambda (n) n))
(define out (lambda (n) n))
(define param (lambda (n) n))
(define xmap '())
(define eqno -1)

;(define inject-boundary
;  (lambda (n) 
;    (if (pair? (member n boundary-list))
;      (string-concatenate 
;        (list "m[" (number->string (car (member n boundary-list))) "]"))
;      (number->string (const n)))))

(define populate-x
  (lambda (lis)
    (for-each (lambda (x) 
                (if (not (assoc x xmap))
                  (set! xmap 
                     (assoc-set! xmap x (list (* 3 (length xmap)) #f)))))
              lis)))

(define x-get
  (lambda (key)
    (car (assoc-ref xmap key))))

(define m-in
  (lambda (n)
    (display (string-concatenate (list " + X[" (number->string n) "]")))))

(define m-out
  (lambda (n)
    (display (string-concatenate (list " - X[" (number->string n) "]")))))

(define p-in
  (lambda (n)
    (display (string-concatenate (list " + X[" (number->string (+ n 1)) "]")))))

(define p-out
  (lambda (n)
    (display (string-concatenate (list " - X[" (number->string (+ n 1)) "]")))))

(define h-in
  (lambda (n)
    (display (string-concatenate 
               (list 
                 " + X[" (number->string n) 
                 "] * X[" (number->string (+ n 2)) "]")))))

(define h-out
  (lambda (n)
    (display (string-concatenate 
               (list 
                 " - X[" (number->string n) 
                 "] * X[" (number->string (+ n 2)) "]")))))
(define Y
  (lambda ()
    (set! eqno (1+ eqno))
    (newline)
    (display 
      (string-concatenate (list 
       "Y[" (number->string eqno) "] = ")))))

(define extract
  (lambda (bound)
    (let ((node (car bound)))
      (for-each 
            (lambda (x) 
              (set! x (symbol->string x))
              (if (string=? x "m")
                (begin (Y) (m-in (x-get node)) 
                  (display (string-concatenate (list
                    " - " x "[" (number->string (x-get node)) "]")))))
              (if (string=? x "p")
                (begin (Y) (p-in (x-get node)) 
                  (display (string-concatenate (list
                    " - " x "[" (number->string (+ 1 (x-get node))) "]")))))
              (if (string=? x "t")
                (begin (Y) (h-in (x-get node)) 
                  (display (string-concatenate (list
                    " - " x "[" (number->string (+ 2 (x-get node))) "]"))))))
            (cdr bound)))))

(define-syntax valve
  (syntax-rules ()
        ((_ media upstream downstream p)
         (begin 
           (populate-x (list upstream downstream))
           (Y) (m-in (x-get upstream)) (m-out (x-get downstream))
           (Y) (h-in (x-get upstream)) (h-out (x-get downstream))
           (Y) (p-in (x-get upstream)) 
               (display " * ") (display (number->string p))
               (p-out (x-get downstream))
           (newline)))))

(define-syntax header
  (syntax-rules ()
        ((_ upstream downstream)
         (begin
           (populate-x (append upstream downstream))
           (Y) (for-each m-in (map x-get upstream)) 
               (for-each m-out (map x-get downstream))
           (Y) (for-each h-in (map x-get upstream)) 
               (for-each h-out (map x-get downstream)) 
           (newline)))))

(define-syntax provide-params
  (syntax-rules ()
        ((_ (node var* ...)* ...)
         (begin
           (for-each extract (list (node var* ...)* ... ))))))


(valve ws (in 1) (out 2) (param 0.99)) 
(header (in '(2 3)) (out '(4)))
(provide-params 
   '(1 p m t)
   '(3 m t))
