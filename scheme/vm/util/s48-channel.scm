; Copyright (c) 1993-2004 by Richard Kelsey and Jonathan Rees. See file COPYING.

; Implementation of OS channels in Scheme.
;
; A channel an index into a vector of ports in the underlying Scheme
; implementation.

; Vector mapping indicies to ports.

(define *channels* '#())

(define (vector-posq vec thing)
  (let loop ((i 0))
    (cond ((= i (vector-length vec))
	   #f)
	  ((eq? thing (vector-ref vec i))
	   i)
	  (else
	   (loop (+ i 1))))))

(define (channel->port channel)
  (vector-ref *channels* channel))

(define input-channel->port channel->port)
(define output-channel->port channel->port)

(define (port->channel port)
  (or (vector-posq *channels* port)
      (make-channel port)))

(define input-port->channel port->channel)
(define output-port->channel port->channel)

; Add PORT to the vector of channels, reusing a slot if possible.

(define (make-channel port)
  (let ((channel (or (vector-posq *channels* #f)
		     (let ((channel (vector-length *channels*)))
		       (set! *channels* (list->vector
					 (append (vector->list *channels*)
						 (list #f #f #f #f))))
		       channel))))
    (vector-set! *channels* channel port)
    channel))

; The default ports

(define (current-input-channel)
  (port->channel (current-input-port)))

(define (current-output-channel)
  (port->channel (current-output-port)))

(define (current-error-channel)
  (port->channel (current-error-port)))

; These just open or close the appropriate port and coerce it to a channel.

(define (open-input-file-channel filename)
  (receive (port status)
      (prescheme:open-input-file filename)
    (if (eq? status (enum prescheme:errors no-errors))
	(values (port->channel port) status)
	(values #f status))))

(define (open-output-file-channel filename)
  (receive (port status)
      (prescheme:open-output-file filename)
    (if (eq? status (enum prescheme:errors no-errors))
	(values (port->channel port) status)
	(values #f status))))

(define (close-input-channel channel)
  (prescheme:close-input-port (channel->port channel)))

(define (close-output-channel channel)
  (prescheme:close-output-port (channel->port channel)))

(define (channel-ready? channel read?)
  (values (if read?
	      (char-ready? (channel->port channel))
	      #t)
	  (enum prescheme:errors no-errors)))

;----------------
; Non-blocking I/O (implemented using CHAR-READY?)
;
; We keep a list of channels for which the user is waiting.  These will
; all be input channels as CHAR-READY? only works on input ports.

(define *pending-channels* '())

(define (channel-read-block channel start count wait?)
  (cond ((char-ready? (channel->port channel))
	 (receive (count eof? status)
	     (read-block (channel->port channel) start count)
	   (values count eof? #f status)))
	(wait?
	 (set! *pending-channels* (cons channel *pending-channels*))
	 (values 0 #f #t (enum prescheme:errors no-errors)))
	(else
	 (values 0 #f #f (enum prescheme:errors no-errors)))))

(define (channel-write-block channel start count)
  (values count #f (write-block (channel->port channel) start count)))

(define (channel-buffer-size) 4096)

(define (channel-abort channel)
  (set! *pending-channels* (delq channel *pending-channels*))
  0)

;----------------
; Events
;
; A keyboard interrupt can be generated by setting the following to #t.

(define *pending-keyboard-interrupt?* #f)

(define (initialize-events)
  (set! *channels* (make-vector 10 #f))
  (set! *pending-channels* '())
  (set! *pending-keyboard-interrupt?* #f))

(define (pending-event?)
  (or *pending-keyboard-interrupt?*
      (any? (lambda (channel)
	      (char-ready? (channel->port channel)))
	    *pending-channels*)))

; The event enumeration is copied from the C version of this code.

(define-enumeration events
  (keyboard-interrupt-event
   io-completion-event
   io-error-event
   alarm-event
   os-signal-event
   error-event
   no-event
   ))

(define (get-next-event)
  (cond (*pending-keyboard-interrupt?*
	 (set! *pending-keyboard-interrupt?* #f)
	 (values (enum events keyboard-interrupt-event) #f #f))
	((any (lambda (channel)
		(char-ready? (channel->port channel)))
	      *pending-channels*)
	 => (lambda (channel)
	      (set! *pending-channels* (delq channel *pending-channels*))
	      (values (enum events io-completion-event)
		      channel
		      0)))
	(else
	 (values (enum events no-event) #f #f))))

(define (wait-for-event max-wait minutes?)
  (breakpoint "Waiting"))


