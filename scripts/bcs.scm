;;; get string for parameter node
(define paramptr
  (lambda (x node)
      (string-append x "[" (number->string node) "]")))

;;; Extract equation for boundary conditions
(define extract
  (lambda (blist) 
    (let ([ node (car blist) ][ e "" ])
      (set! eqns (append eqns 
        (map (lambda (x) (set! x (symbol->string x))
          (cond
            [(string=? x "m") 
                (string-append 
                  (X_i node "m") "-" (paramptr x (/ (x-get node) 3)))]
            [(string=? x "p") 
                (string-append
                  (X_i node "p") "-" (paramptr x (/ (x-get node) 3)))]
            [(string=? x "t") 
                (string-append 
                  (X_i node "h") "-"
                  "h_pT(&"  (X_i node "p") ",&"
                  (paramptr x (/ (x-get node) 3)) ")")]))
        (cdr blist))))

      (set! J (append J 
        (map (lambda (x) (set! x (symbol->string x))
          (cond
            [(string=? x "m") 
                (string-append 
                  "{{df}}[" (number->string (x-get node)) "]=+1;")]
            [(string=? x "p") 
                (string-append
                  "{{df}}[" (number->string (+ 1 (x-get node))) "]=+1;")]
            [(string=? x "t") 
                (string-append 
                  "{{df}}[" (number->string (+ 2 (x-get node))) "]=+1;")]))
        (cdr blist)))))))




;;; Macro for injecting boundary conditions into system
(define-syntax provide-params
  (syntax-rules ()
        ((_ (node var* ...)* ...)
         (begin
           (for-each extract (list (node var* ...)* ... ))))))


