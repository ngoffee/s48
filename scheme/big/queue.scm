; Copyright (c) 1993-2001 by Richard Kelsey and Jonathan Rees. See file COPYING.

; Queues
; Richard's code with Jonathan's names.
;
;     Richard's names:     Jonathan's names (modified by popular demand):
;      make-empty-queue     make-queue
;      add-to-queue!	    enqueue!
;      remove-from-queue!   dequeue!
;
; Now using optimistic concurrency.  We really need two sets of procedures to
; allow those who don't care to avoid the cost of the concurrency checks.

(define-synchronized-record-type queue :queue
  (really-make-queue uid head tail)
  (head tail)		; synchronize on these
  queue?
  (uid queue-uid)
  (head queue-head set-queue-head!)
  (tail queue-tail set-queue-tail!))

(define queue-uid (list 0))

(define (next-uid)
  (atomically
    (let ((uid (provisional-car queue-uid)))
      (provisional-set-car! queue-uid (+ uid 1))
      uid)))

(define (make-queue)
  (really-make-queue (next-uid) '() '()))

; The procedures for manipulating queues.

(define (queue-empty? q)
  (null? (queue-head q)))

(define (enqueue! q v)
  (ensure-atomicity!
    (let ((p (cons v '())))
      (cond ((null? (queue-head q))
	     (set-queue-head! q p))
	    ((null? (queue-tail q))		; someone got in first
	     (invalidate-current-proposal!))
	    (else
	     (set-cdr! (queue-tail q) p)))
      (set-queue-tail! q p))))

(define (queue-front q)
  (ensure-atomicity
    (if (queue-empty? q)
	(error "queue is empty" q)
	(car (queue-head q)))))

(define (dequeue! q)
  (ensure-atomicity
    (let ((pair (queue-head q)))
      (cond ((null? pair)	;(queue-empty? q)
	     (error "empty queue" q))
	    (else
	     (let ((value (car pair))
		   (next  (cdr pair)))
	       (set-queue-head! q next)
	       (if (null? next)
		   (set-queue-tail! q '()))   ; don't retain pointers
	       value))))))

(define (empty-queue! q)
  (ensure-atomicity
    (set-queue-head! q '())
    (set-queue-tail! q '())))

(define (on-queue? v q)
  (memq v (queue-head q)))

; This removes the first occurrence of V from Q.

(define (delete-from-queue! q v)
  (delete-from-queue-if! q
			 (lambda (x)
			   (eq? x v))))

(define (delete-from-queue-if! q pred)
  (ensure-atomicity
    (let ((list (queue-head q)))
      (cond ((null? list)
	     #f)
	    ((pred (car list))
	     (set-queue-head! q (cdr list))
	     (if (null? (cdr list))
		 (set-queue-tail! q '()))   ; don't retain pointers
	     #t)
	    ((null? (cdr list))
	     #f)
	    (else
	     (let loop ((list list))
	       (let ((tail (cdr list)))
		 (cond ((null? tail)
			#f)
		       ((pred (car tail))
			(set-cdr! list (cdr tail))
			(if (null? (cdr tail))
			    (set-queue-tail! q list))
			#t)
		       (else
			(loop tail))))))))))

(define (queue->list q)
  (ensure-atomicity
    (map (lambda (x) x)
	 (queue-head q))))

(define (list->queue list)
  (if (null? list)
      (make-queue)
      (let ((head (cons (car list) '())))
	(let loop ((rest (cdr list)) (tail head))
	  (if (null? rest)
	      (really-make-queue (next-uid) head tail)
	      (begin
		(let ((next (cons (car rest) '())))
		  (set-cdr! tail next)
		  (loop (cdr rest) next))))))))

(define (queue-length q)
  (ensure-atomicity
    (length (queue-head q))))

