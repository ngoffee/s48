; Copyright (c) 1993 by Richard Kelsey and Jonathan Rees.  See file COPYING.


; Hash table package that allows for different hash and comparison functions.

(define-record-type table table-type
  (really-make-table size data ref set)
  table?
  (size table-size set-table-size!)
  (data table-data set-table-data!)
  (ref table-ref-procedure set-table-ref-procedure!)
  (set table-set!-procedure set-table-set!-procedure!))

(define (table-ref table key)
  ((table-ref-procedure table) table key))

(define (table-set! table key value)
  ((table-set!-procedure table) table key value))

; These numbers are guesses
(define linear-table-size-limit 15)
(define table-size-limit 100000)

(define (next-table-size count)		; Figure out next good size for table.
  (let ((new-size (+ (* count 3) 1)))
    (if (>= new-size table-size-limit)
	(error "requested table size is too large" new-size))
    new-size))

(define (make-table-maker comparison-function hash-function)
  (let* ((assoc (make-assoc comparison-function))
	 (ref-proc (make-linear-table-ref assoc))
	 (x->hash-table! (make->hash-table assoc hash-function))
	 (set!-proc (make-linear-table-set! assoc x->hash-table!)))
    (lambda ()
      (really-make-table 0 #f ref-proc set!-proc))))

; Speed & size hack?!  See how well it works out, then revert to
; a-lists if it doesn't.

(define null-entry #f)

(define (new-entry key val others)	;(cons (cons key val) others)
  (let ((v (make-vector 3 #f)))
    (vector-set! v 0 key)
    (vector-set! v 1 val)
    (vector-set! v 2 others)
    v))

(define (make-assoc pred)
  (if (eq? pred eq?)
      eq?-assoc
      (lambda (thing alist)
	(let loop ((alist alist))
	  (cond ((not alist)
		 #f)
		((pred thing (vector-ref alist 0))
		 alist)
		(else
		 (loop (vector-ref alist 2))))))))

(define eq?-assoc
  (lambda (thing alist)
    (let loop ((alist alist))
      (cond ((not alist)
	     #f)
	    ((eq? thing (vector-ref alist 0))
	     alist)
	    (else
	     (loop (vector-ref alist 2)))))))

; Turn some version of ASSOC into a table reference procedure for a-list
; tables.
(define (make-linear-table-ref assoc)
  (lambda (table key)
    (let ((probe (assoc key (table-data table))))
      (if probe (vector-ref probe 1) #f))))

; Turn some version of ASSOC and a hash function into a table set! procedure
; for a-list tables.  If the table gets too big it is turned into a hash table.
(define (make-linear-table-set! assoc x->hash-table!)
  (lambda (table key value)
    (let* ((data (table-data table))
	   (probe (assoc key data)))
      (cond (probe
	     (vector-set! probe 1 value))
	    (else
	     (set-table-data! table (new-entry key value data))
	     (let ((size (table-size table)))
	       (if (< size linear-table-size-limit)
		   (set-table-size! table (+ size 1))
		   (x->hash-table! table size))))))))

; Return a function to transform linear tables into hash tables.
(define (make->hash-table assoc hash-function)
  (let ((hash-table-ref (make-hash-table-ref assoc hash-function))
	(hash-table-set! (make-hash-table-set! assoc hash-function)))
    (lambda (table size)
      (let ((data (table-data table)))
	(set-table-ref-procedure! table hash-table-ref)
	(set-table-set!-procedure! table hash-table-set!)
	(table-expand-table! table (next-table-size size))
	(table-enter-alist! table data)))))

(define (make-hash-table-ref assoc hash-function)
  (lambda (table key)
    (let* ((data (table-data table))
	   (h (modulo (hash-function key)
		      (vector-length data)))
	   (alist (vector-ref data h))
	   (probe (assoc key alist)))
      (if probe (vector-ref probe 1) #f))))
	       
(define (make-hash-table-set! assoc hash-function)
  (lambda (table key value)
    (let* ((data (table-data table))
	   (h (modulo (hash-function key)
		      (vector-length data)))
	   (alist (vector-ref data h))
	   (probe (assoc key alist)))
      (cond (probe
	     (vector-set! probe 1 value))
	    (else
	     (vector-set! data h (new-entry key value alist))
	     (let ((size (+ (table-size table) 1)))
	       (if (< size (vector-length data))
		   (set-table-size! table size)
		   (expand-hash-table! table size))))))))

(define (expand-hash-table! table size)
  (let ((data (table-data table)))
    (table-expand-table! table (next-table-size size))
    (do ((i 0 (+ i 1)))
	((>= i (vector-length data)))
      (table-enter-alist! table (vector-ref data i)))))

(define (table-enter-alist! table alist)
  (let ((set!-proc (table-set!-procedure table)))
    (do ((alist alist (vector-ref alist 2)))
	((not alist))
      (set!-proc table (vector-ref alist 0) (vector-ref alist 1)))))

(define (table-expand-table! table size)
  (set-table-size! table 0)
  (if (< size table-size-limit)
      (set-table-data! table (make-vector size #f))
      (error "requested table size is too large" size)))

(define (table-walk proc table)
  (let ((data (table-data table)))
    (cond ((not data))
	  ((= 3 (vector-length data))
	   (alist-walk proc data))
	  (else
	   (do ((i 0 (+ i 1)))
	       ((>= i (vector-length data)))
	     (alist-walk proc (vector-ref data i)))))))

(define (alist-walk proc alist)
  (do ((alist alist (vector-ref alist 2)))
      ((not alist))
    (proc (vector-ref alist 0) (vector-ref alist 1))))

(define (table->entry-list table)
  (let ((list '()))
    (table-walk (lambda (k v)
		  (set! list (cons v list)))
		table)
    list))

; Actual tables

; The default hash function only on works on things that would work in
; a CASE expression.  Even then, numbers don't really "work," since
; they are compared using eq?.

(define (default-table-hash-function obj)
  (cond ((symbol? obj) (string-hash (symbol->string obj)))
	((integer? obj) obj)
	((char? obj) (+ 333 (char->integer obj)))
	((eq? obj #f) 3001)
	((eq? obj #t) 3003)
	((null? obj) 3005)
	(else (error "value cannot be used as a table key" obj))))

(define string-hash (structure-ref features string-hash))

(define (symbol-hash symbol)
  (string-hash (symbol->string symbol)))

(define make-table         (make-table-maker eq? default-table-hash-function))
(define make-string-table  (make-table-maker string=? string-hash))
(define make-symbol-table  (make-table-maker eq?      symbol-hash))
(define make-integer-table (make-table-maker =	      (lambda (x) x)))
