; -*- Mode: Scheme; Syntax: Scheme; Package: Scheme; -*-
; Copyright (c) 1992 by Richard Kelsey and Jonathan Rees.  See file COPYING.

; These are hacked to ensure that all calls to INPUT-TYPE-PREDICATE and
; INPUT-TYPE-COERCION are evaluated at load time (because they don't
; have readily reconstructed types).

(define-syntax define-primitive
  (lambda (exp rename compare)
    (destructure (((d-p opcode input-types action . returner-option) exp))
      (let ((proc (rename 'proc)))
      `(let ((,proc
	      (primitive-procedure-action ,input-types #f ,action . ,returner-option)))
	   (define-opcode ,opcode (,proc)))))))

(define-syntax define-consing-primitive
  (lambda (exp rename compare)
    (destructure (((d-c-p opcode input-types space-proc action . returner-option)
		   exp))
      (let ((proc (rename 'proc)))
	`(let ((,proc
		(primitive-procedure-action ,input-types ,space-proc ,action . ,returner-option)))
	   (define-opcode ,opcode (,proc)))))))
  
(define-syntax primitive-procedure-action
  (lambda (exp rename compare)
    (destructure (((p-p-b input-types space-proc action . returner-option) exp))
      (let* ((shorten (lambda (l1 l2) (map (lambda (x1 x2) x2 x1) l1 l2)))
	     (places (reverse (shorten '(*val* arg2 arg3 arg4) input-types)))
	     (preds (reverse (shorten '(pred1 pred2 pred3 pred4) input-types)))
	     (x->ys (reverse (shorten '(x->y1 x->y2 x->y3 x->y4) input-types)))
	     (nargs (length input-types)))
	`(let (,@(map (lambda (type pred) `(,pred (input-type-predicate ,type)))
		      input-types
		      preds)
	       ,@(map (lambda (type x->y) `(,x->y (input-type-coercion ,type)))
		      input-types
		      x->ys))
	   (lambda ()
	     (let* (,@(if space-proc
			  `((key (ensure-space (,space-proc *val*))))
			  '())
		    ,@(if (>= nargs 2) `((arg2 (pop))) `())
		    ,@(if (>= nargs 3) `((arg3 (pop))) `())
		    ,@(if (>= nargs 4) `((arg4 (pop))) `())
		    )
	       (if (and ,@(map (lambda (pred place)
				 `(,pred ,place))
			       preds
			       places))
		   ,(let ((yow `(,action
				 ,@(map (lambda (x->y place)
					  `(,x->y ,place))
					x->ys
					places)
				 ,@(if space-proc '(key) '()))))
		      (if (null? returner-option)
			  yow
			  `(goto ,(car returner-option) ,yow)))
		   (goto ,(case nargs
			    ((0) 'raise-exception)
			    ((1) 'raise-exception1)
			    ((2) 'raise-exception2)
			    ((3) 'raise-exception3)
			    ((4) 'raise-exception4))
			 0 . ,places)))))))))


