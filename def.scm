;;; Customized model definition 
(valve ws (in 1) (out 2) (param 0.99)) 
(header (in '(2 3)) (out '(4 5)))
(provide-params 
   '(1 p m t)
   '(3 m t)
   '(5 m))
 
