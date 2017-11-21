;;; bcs-inject.scm
;;; Injection of boundary conditions into system of equations
;;; according model definition
;;; 
;;; This file is part of Thermograph.
;;; 
;;; Thermograph is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;; 
;;; Thermograph is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with Thermograph. If not, see <http://www.gnu.org/licenses/>.
;;; 
;;; Copyright 2016 Martin Michel

;;; Macro for injecting boundary conditions into system
(define-syntax set-bcs!
  (syntax-rules ()
                ((_ (node var* ...)* ...)
                 (begin
                   (for-each extract (list (node var* ...)* ... ))))))


;;; get string for parameter node
(define paramptr
  (lambda (x node)
    (string-append x "[" (number->string node) "]")))

;;; Decrement remaining X nodes
(define decr-xmap
  (lambda (node)
    (for-each (lambda (ptr) 
                (set! xmap
                  (assoc-set! xmap ptr (cons (car (assq-ref xmap ptr)) 
                                             (map (lambda (x) 
                                                    (if (integer? x) (- x 1) x)) 
                                                  (cdr (assq-ref xmap ptr))))))) 
              (cdr (memq node (reverse (map (lambda(y) (car y) ) xmap))))) ))

;;; Provide IF97 h_pT call with replaced arguments
(define h-from-new
  (lambda (node x)
    (string-append "h_pT(&"  
                   (if (integer? (cadr (slice node))) 
                     (string-append "X[" (number->string (cadr (slice node))) "]") 
                     (cadr (slice node))) 
                   ",&" (paramptr x (/ (x-get node) 3)) ")")))

;;; Extract equation for boundary conditions
(define extract
  (lambda (blist) 
    (let* ([ node (car blist) ] )
      (map (lambda (x) (set! x (symbol->string x))
             (set! nbcs (+ 1 nbcs))
             (cond
               [(string=? x "m")
                ; local decrement of current slice
                (set-cdr! (slice node) 
                          (map (lambda (x) (if (integer? x) (- x 1) x)) 
                               (cdr (slice node))))
                ; set first entry [-->"m"] to parameter
                (set-car! (slice node) (paramptr x (/ (x-get node) 3)))
                ; update the xmap entry for this "m" key
                (set! xmap
                  (assoc-set! xmap node (cons (x-get node) (slice node))))
                ; now decrease all other X entries by one
                (decr-xmap node)]

               [(string=? x "p")
                (set-cdr! (cdr (slice node)) 
                          (map (lambda (x) (if (integer? x) (- x 1) x)) 
                               (cddr (slice node))))
                (set-car! (cdr (slice node)) (paramptr x (/ (x-get node) 3)))
                (set! xmap
                  (assoc-set! xmap node (cons (x-get node) (slice node))))
                (decr-xmap node)]

               [(string=? x "t") 
                ; local decrement of current slice not required
                ; because "h" is already last slice entry!
                ; …continue with exchange X entry with parameter:
                (set-car! (cddr (slice node)) (h-from-new node x))
                (set! xmap
                  (assoc-set! xmap node (cons (x-get node) (slice node))))
                (decr-xmap node)]))
           (cdr blist)) )))

