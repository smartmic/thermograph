;;; valve-inject.scm
;;; Extracting equations for thermodynamic component "valve".
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

;;; Macro for valve component
(define-syntax valve
  (syntax-rules ()
    ((_ media upstrm downstrm prm)
       (begin
           (populate-x (list upstrm downstrm))

           ;; Append component equations to global set of equations
           (set! eqns (append eqns (list 
            
           ; (i) mass balance
           (string-append
             (X_i upstrm "m") "-" (X_i downstrm "m"))
           ; (ii) isenthalpic process
           (string-append
             (X_i upstrm "h") "-" (X_i downstrm "h"))
           ; (iii) pressure relation (relative dp by parameter)
           (string-append
             (X_i upstrm "p") "*" (number->string prm) "-"
               (X_i downstrm "p"))))) 


           ;; Append to Jacobian: 
           (set! J (append J (list 

           ; (i) mass balance
           (string-append
               (if (integer? (car (slice upstrm))) 
                   (string-append 
                     "{{df}}["(number->string (car (slice upstrm)))"]=+1;")
                   "")
               (if (integer? (car (slice downstrm))) 
                   (string-append
                   "{{df}}["(number->string (car (slice downstrm)))"]=-1;")
                   ""))
           ; (ii) isenthalpic process
           (string-append
               (if (integer? (caddr (slice upstrm))) 
                   (string-append 
                     "{{df}}["(number->string (caddr (slice upstrm)))"]=+1;")
                   "")
               (if (integer? (caddr (slice downstrm))) 
                   (string-append
                   "{{df}}["(number->string (caddr (slice downstrm)))"]=-1;")
                   ""))
           ; (iii) pressure relation (relative dp by parameter)
           (string-append
               (if (integer? (cadr (slice upstrm))) 
                   (string-append
                   "{{df}}["(number->string (cadr (slice upstrm)))"]="
                     (number->string prm)";")
                   "")
               (if (integer? (cadr (slice downstrm))) 
                   (string-append
                   "{{df}}["(number->string (cadr (slice downstrm)))"]=-1;")
                   "")) ))) ))))
