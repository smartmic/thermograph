
;;; various definitions
(define ws "water")
(define nbcs 0)
(define boundary-list '(0 2))
(define p "p")
(define m "m")
(define t "t")
(define in (lambda (n) n))
(define out (lambda (n) n))
(define param (lambda (n) n))
(define xmap '())
(define eqns '())
(define J '())

;;; setup and populate association list for mapping user node id to 
;;; X array index
(define populate-x
  (lambda (lis)
    (for-each (lambda (x) 
                (if (not (assoc x xmap))
                (set! xmap (assoc-set! xmap x 
                (cons (* 3 (length xmap)) (iota 3 (* 3 (length xmap)) 1))))))
              lis)))

;;; get internal key (index) from user supplied node id
(define x-get
  (lambda (key)
    (car (assoc-ref xmap key))))

;;; Extraction of slice (m p h) for given node from xmap
(define slice
  (lambda (key)
    (cdr (assq-ref xmap key))))

;;; get numbered X node
(define X_i
  (lambda (node param)
      (let ([entry #f])
        (cond 
        [(string=? param "m") (set! entry (car (slice node)))]
        [(string=? param "p") (set! entry (cadr (slice node)))]
        [(string=? param "h") (set! entry (caddr (slice node)))])
      (if (integer? entry) 
      (string-append "X[" (number->string entry) "]") 
      entry) )))

