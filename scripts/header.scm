;;; header.scm
;;; Extraction of equations for header (multi-mixer-splitter)
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

;;; Macro for header component (multi-mixer-splitter)
(define-syntax header
  (syntax-rules ()
        ((_ upstrm downstrm)
         (begin
           (populate-x (append upstrm downstrm))

           ; append component equations to global set of equations
           (set! eqns (append eqns (append 
            ; form list of equations as one-line strings
            (list (string-concatenate (append

             ; mass balance
             (map (lambda (x) 
                    (string-append "+" (X_i x "m"))) (map x-get upstrm)) 
             (map (lambda (x) 
                    (string-append "-" (X_i x "m"))) (map x-get downstrm)))))

             ; pressure identity (part 1): pressure of all inflow
             ; states equals the pressure of the first outflow state
             (map (lambda (x) 
               (string-append
                   "+" (X_i x "p") "-" (X_i (x-get (car downstrm)) "p")))
             (map x-get upstrm))

             ; pressure identity (part 2): the pressure of all outflow
             ; states is equal
             (map (lambda (x) 
               (string-append
                   "+" (X_i (x-get (car downstrm)) "p") "-" (X_i x "p")))
             (map x-get (cdr downstrm)))

             ; enthalpy identity: the enthalpy of all outflow states
             ; is equal
             (map (lambda (x) 
               (string-append
                   "+" (X_i (x-get (car downstrm)) "h") "-" (X_i x "h")))
             (map x-get (cdr downstrm)))

             ; energy balance
             (list (string-concatenate (append
               (map (lambda (x) 
                   (string-append "+" (X_i x "m") "*" (X_i x "h")))
                     (map x-get upstrm)) 
               (map (lambda (x) 
                   (string-append "-" (X_i x "m") "*" (X_i x "h")))
                     (map x-get downstrm))))) )))

           (set! J (append J (append 
            ; form list of equations as one-line strings
            (list (string-concatenate (append

             ; mass balance
             (map (lambda (x) 
                    (string-append 
                      "{{df}}["(number->string x)"]=+1;")) 
                  (map x-get upstrm)) 
             (map (lambda (x) 
                    (string-append 
                      "{{df}}["(number->string x)"]=-1;")) 
                  (map x-get downstrm)))))

             ; pressure identity (part 1): pressure of all inflow
             ; states equals the pressure of the first outflow state
             (map (lambda (x) 
               (string-append
                   "{{df}}["(number->string (+ 1 x))"]=+1;" 
                   "{{df}}["(number->string (+ 1 (x-get (car downstrm))))"]=-1;"))
             (map x-get upstrm))

             ; pressure identity (part 2): the pressure of all outflow
             ; states is equal
             (map (lambda (x) 
               (string-append
                   "{{df}}["(number->string (+ 1 (x-get (car downstrm))))"]=+1;"
                   "{{df}}["(number->string (+ 1 x))"]=-1;"))
             (map x-get (cdr downstrm)))

             ; enthalpy identity: the enthalpy of all outflow states
             ; is equal
             (map (lambda (x) 
               (string-append
                   "{{df}}["(number->string (+ 2 (x-get (car downstrm))))"]=+1;"
                   "{{df}}["(number->string (+ 2 x))"]=-1;"))
             (map x-get (cdr downstrm)))

             ; energy balance
             (list (string-concatenate (append
               (map (lambda (x) 
                   (string-append 
                     "{{df}}[" (number->string x) "]=+" (X_i x "h") ";"
                     "{{df}}[" (number->string (+ 2 x)) "]=+" (X_i x "m") ";" ))
                     (map x-get upstrm)) 
               (map (lambda (x) 
                   (string-append 
                     "{{df}}[" (number->string x) "]=+" (X_i x "h") ";"
                     "{{df}}[" (number->string (+ 2 x)) "]=-" (X_i x "m") ";"))
                     (map x-get downstrm))))) )))))))
