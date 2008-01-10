; Copyright (c) 1993-2008 by Richard Kelsey and Jonathan Rees. See file COPYING.


(define-syntax loophole
  (syntax-rules ()
    ((loophole ?type ?form)
     (begin (lambda () ?type)    ;Elicit unbound-variable warnings, etc.
	    ?form))))
    
