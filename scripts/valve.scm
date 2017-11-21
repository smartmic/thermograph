;;; valve.scm
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

           ; append component equations to global set of equations
           (set! eqns (append eqns (list 
            
           ; mass balance
           (string-append
             (X_i (x-get upstrm) "m") "-" (X_i (x-get downstrm) "m"))
           ; isenthalpic process
           (string-append
             (X_i (x-get upstrm) "h") "-" (X_i (x-get downstrm) "h"))
           ; pressure relation (relative dp by parameter)
           (string-append
             (X_i (x-get upstrm) "p") "*" (number->string prm) "-"
               (X_i (x-get downstrm) "p")))))
           ; append component equations to global set of equations
           (set! J (append J (list 
            
           ; mass balance
           (string-append 
             "{{df}}["(number->string (x-get upstrm))"]=+1;"
             "{{df}}["(number->string (x-get downstrm))"]=-1;")
           ; isenthalpic process
           (string-append 
             "{{df}}["(number->string (+ 2 (x-get upstrm)))"]=+1;"
             "{{df}}["(number->string (+ 2 (x-get downstrm)))"]=-1;")
           ; pressure relation (relative dp by parameter)
           (string-append
             "{{df}}["(number->string (+ 1 (x-get upstrm)))"]="
             (number->string prm)";"
             "{{df}}["(number->string (+ 1 (x-get downstrm)))"]=-1;"))))))))


