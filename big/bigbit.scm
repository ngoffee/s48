; Copyright (c) 1993, 1994 Richard Kelsey and Jonathan Rees.  See file COPYING.

; Bitwise logical operators on bignums.


(define-opcode-extension bitwise-not &bitwise-not)
(define-opcode-extension bitwise-and &bitwise-and)
(define-opcode-extension bitwise-ior &bitwise-ior)
(define-opcode-extension bitwise-xor &bitwise-xor)
(define-opcode-extension arithmetic-shift &arithmetic-shift)


(define (integer-bitwise-not m)
  ;; (integer+ (integer-negate m) -1)
  (integer- -1 m))

(define (integer-bitwise-and m n)
  (if (or (integer= 0 m) (integer= 0 n))
      0
      (integer-bitwise-op bitwise-and m n)))

(define (integer-bitwise-ior m n)
  (cond ((integer= 0 m) n)
	((integer= 0 n) m)
	(else
	 (integer-bitwise-op bitwise-ior m n))))

(define (integer-bitwise-xor m n)
  (cond ((integer= 0 m) n)
	((integer= 0 n) m)
	(else
	 (integer-bitwise-op bitwise-xor m n))))

(define (integer-bitwise-op op m n)
  (let ((m (integer->bignum m))
	(n (integer->bignum n)))
    (let ((finish (lambda (sign-bit mag-op)
		    (let ((mag (mag-op op
				       (bignum-magnitude m)
				       (bignum-magnitude n))))
		      (make-integer (if (= 0 sign-bit) 1 -1)
				    (if (= 0 sign-bit)
					mag
					(negate-magnitude mag)))))))
      (if (>= (bignum-sign m) 0)
	  (if (>= (bignum-sign n) 0)
	      (finish (op 0 0) magnitude-bitwise-binop-pos-pos)
	      (finish (op 0 1) magnitude-bitwise-binop-pos-neg))
	  (if (>= (bignum-sign n) 0)
	      (finish (op 0 1) magnitude-bitwise-binop-neg-pos)
	      (finish (op 1 1) magnitude-bitwise-binop-neg-neg))))))

(define radix-mask (- radix 1))

(define (magnitude-bitwise-binop-pos-pos op m n)
  (let recur ((m m) (n n))
    (if (and (zero-magnitude? m) (zero-magnitude? n))
	m
	(adjoin-digit (bitwise-and (op (low-digit m) (low-digit n)) radix-mask)
		      (recur (high-digits m) (high-digits n))))))

; Same as the above, except that one magnitude is that of negative number.

(define (magnitude-bitwise-binop-neg-pos op m n)
  (magnitude-bitwise-binop-pos-neg op n m))

(define (magnitude-bitwise-binop-pos-neg op m n)
  (let recur ((m m) (n n) (carry 1))
    (if (and (zero-magnitude? n) (zero-magnitude? m))
	(integer->magnitude (op 0 carry))
	(let ((n-digit (negate-low-digit n carry)))
	  (adjoin-digit (bitwise-and (op (low-digit m) n-digit) radix-mask)
			(recur (high-digits m)
			       (high-digits n)
			       (if (>= n-digit radix) 1 0)))))))

; Now both M and N are magnitudes of negative numbers.

(define (magnitude-bitwise-binop-neg-neg op m n)
  (let recur ((m m) (n n) (m-carry 1) (n-carry 1))
    (if (and (zero-magnitude? n) (zero-magnitude? m))
	(integer->magnitude (op m-carry n-carry))
	(let ((m-digit (negate-low-digit m m-carry))
	      (n-digit (negate-low-digit n n-carry)))
	  (adjoin-digit (bitwise-and (op m-digit n-digit) radix-mask)
			(recur (high-digits m)
			       (high-digits n)
			       (if (>= m-digit radix) 1 0)
			       (if (>= n-digit radix) 1 0)))))))

(define (negate-low-digit m carry)
  (+ (bitwise-and (bitwise-not (low-digit m))
		  radix-mask)
     carry))

(define (negate-magnitude m)
  (let recur ((m m) (carry 1))
    (if (zero-magnitude? m)
	(integer->magnitude carry)
	(let ((res (negate-low-digit m carry)))
	  (if (>= res radix)
	      (adjoin-digit (- res radix)
			    (recur (high-digits m) 1))
	      (adjoin-digit res
			    (recur (high-digits m) 0)))))))

; arithmetic-shift

(define (integer-arithmetic-shift m n)
  (let ((m (integer->bignum m)))
    (make-integer (bignum-sign m)
		  (cond ((> n 0)
			 (shift-left-magnitude (bignum-magnitude m) n))
			((= 1 (bignum-sign m))
			 (shift-right-pos-magnitude (bignum-magnitude m) n))
			(else
			 (shift-right-neg-magnitude (bignum-magnitude m) n))))))

(define (shift-left-magnitude mag n)
  (if (< n log-radix)
      (let ((mask (- (arithmetic-shift 1 (- log-radix n)) 1)))
	(let recur ((mag mag)
		    (low 0))
	  (if (zero-magnitude? mag)
	      (adjoin-digit low zero-magnitude)
	      ;; Split the low digit into left and right parts, and shift
	      (let ((left (arithmetic-shift (low-digit mag)
					    (- n log-radix))) ;shift right
		    (right (arithmetic-shift (bitwise-and (low-digit mag) mask)
					     n)))
		(adjoin-digit (bitwise-ior low right)
			      (recur (high-digits mag)
				     left))))))
      (adjoin-digit 0 (shift-left-magnitude mag (- n log-radix)))))

(define (shift-right-pos-magnitude mag n)
  (if (> n (- 0 log-radix))
      (let ((mask (- (arithmetic-shift 1 (- 0 n)) 1)))
	(let recur ((mag mag))
	  (let ((low (low-digit mag))
		(high (high-digits mag)))
	    (adjoin-digit
	     (bitwise-ior (arithmetic-shift low n)
			  (arithmetic-shift (bitwise-and mask (low-digit high))
					    (+ n log-radix)))
	     (if (zero-magnitude? high)
		 zero-magnitude
		 (recur high))))))
      (shift-right-pos-magnitude (high-digits mag) (+ n log-radix))))
      
(define (shift-right-neg-magnitude mag n)
  (negate-magnitude
   (let digit-recur ((mag mag) (n n) (carry 1))
     (let* ((low (negate-low-digit mag carry))
	    (next-carry (if (>= low radix) 1 0)))
       (if (<= n (- 0 log-radix))
	   (digit-recur (high-digits mag) (+ n log-radix) next-carry)
	   (let ((mask (- (arithmetic-shift 1 (- 0 n)) 1)))
	     (let recur ((mag mag) (n n) (carry carry))
	       (let* ((low (negate-low-digit mag carry))
		      (carry (if (>= low radix) 1 0))
		      (high (high-digits mag))
		      (next (negate-low-digit high carry)))
		 (adjoin-digit
		  (bitwise-ior (arithmetic-shift low n)
			       (arithmetic-shift (bitwise-and mask next)
						 (+ n log-radix)))
		  (if (zero-magnitude? high)
		      (integer->magnitude carry)
		      (recur high n carry)))))))))))

(define log-radix
  (do ((i 0 (+ i 1))
       (r 1 (* r 2)))
      ((>= r radix) i)))

;(define (tst)
;  (let* ((m (random))
;         (n (bitwise-and m 63))
;         (m1 (integer-arithmetic-shift
;              (integer-arithmetic-shift m n)
;              (- 0 n))))
;    (list n m m1 (= m m1))))
;(define random (make-random 17))


(define-method &bitwise-not ((n :integer)) (integer-bitwise-not n))

(define-method &bitwise-and ((n1 :exact-integer) (n2 :exact-integer))
  (integer-bitwise-and n1 n2))
(define-method &bitwise-ior ((n1 :exact-integer) (n2 :exact-integer))
  (integer-bitwise-ior n1 n2))
(define-method &bitwise-xor ((n1 :exact-integer) (n2 :exact-integer))
  (integer-bitwise-xor n1 n2))

(define-method &arithmetic-shift ((n1 :exact-integer) (n2 :exact-integer))
  (integer-arithmetic-shift n1 n2))
