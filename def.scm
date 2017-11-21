;;; def.scm
;;; Definition of model and boundary conditions in Thermograph's DSL
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

(valve ws (in 1) (out 2) (param 0.99)) 
(header (in '(2 3)) (out '(4 5)))

(set-bcs!
   '(1 p m t)
   '(3 m t)
   '(5 m))

(set! eqns '())
(set! J '())

; the following repetiton is still required to …
(valve ws (in 1) (out 2) (param 0.99)) 
(header (in '(2 3)) (out '(4 5)))
