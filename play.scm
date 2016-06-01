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

(define x-io
  (lambda (node sign param)
    (let ((i 0))
      (cond 
        [(string=? param "m") (set! i 0)]
        [(string=? param "p") (set! i 1)]
        [(string=? param "h") (set! i 2)])
      (display 
        (string-concatenate 
          (list (string sign) "X[" (number->string (+ i node)) "]"))))))

(define Y
  (lambda ()
    (set! eqno (1+ eqno))
    (newline)
    (display 
      (string-concatenate (list 
       "Y[" (number->string eqno) "] = ")))))

(define paramptr
  (lambda (x node)
      (display (string-concatenate (list
        " - " x "[" (number->string node) "]")))))

(define extract
  (lambda (blist) 
    (let ((node (car blist)))
      (for-each 
        (lambda (x) 
          (set! x (symbol->string x))
          (cond
            [(string=? x "m") (begin 
                (Y) (x-io (x-get node) #\+ "m") (paramptr x (x-get node)))]
            [(string=? x "p") (begin
                (Y) (x-io (x-get node) #\+ "p") (paramptr x (x-get node)))]
            [(string=? x "t") (begin
                (Y) (x-io (x-get node) #\+ "h") (paramptr x (x-get node)))]))
        (cdr blist)))))

(define-syntax valve
  (syntax-rules ()
    ((_ media upstrm downstrm prm)
     (begin 
       (populate-x (list upstrm downstrm))
       (Y) (x-io (x-get upstrm) #\+ "m") (x-io (x-get downstrm) #\- "m")
       (Y) (x-io (x-get upstrm) #\+ "h") (x-io (x-get downstrm) #\- "h")
       (Y) (x-io (x-get upstrm) #\+ "p") 
           (display "*") (display (number->string prm))
           (x-io (x-get downstrm) #\- "p")
       (newline)))))

(define-syntax header
  (syntax-rules ()
        ((_ upstrm downstrm)
         (begin
           (populate-x (append upstrm downstrm))
           (Y) (for-each (lambda (x) (x-io x  #\+ "m")) (map x-get upstrm)) 
               (for-each (lambda (x) (x-io x  #\- "m")) (map x-get downstrm)) 
           (for-each (lambda (x) 
                       (Y) (x-io x #\+ "p") (x-io (x-get (car downstrm)) #\- "p")) 
                     (map x-get upstrm))
           (for-each (lambda (x) 
                       (Y) (x-io (x-get (car downstrm)) #\+ "p") (x-io x #\- "p")) 
                     (map x-get (cdr downstrm)))
           (Y) (for-each (lambda (x) 
                           (x-io x  #\+ "m")(display "*")(x-io x  #\si  "h")) 
                         (map x-get upstrm)) 
               (for-each (lambda (x) 
                           (x-io x  #\+ "m")(display "*")(x-io x  #\si "h")) 
                         (map x-get downstrm)) 
           (newline)))))

(define-syntax provide-params
  (syntax-rules ()
        ((_ (node var* ...)* ...)
         (begin
           (for-each extract (list (node var* ...)* ... ))))))

(valve ws (in 1) (out 2) (param 0.99)) 
(header (in '(2 3)) (out '(4 5)))
(provide-params 
   '(1 p m t)
   '(3 m t)
   '(5 m))

(newline)
(display xmap)
