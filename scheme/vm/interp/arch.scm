; -*- Mode: Scheme; Syntax: Scheme; Package: Scheme; -*-
; Copyright (c) 1993-2001 by Richard Kelsey and Jonathan Rees. See file COPYING.

; This is file arch.scm.

;;;; Architecture description

(define architecture-version "Vanilla 24")

; Things that the VM and the runtime system both need to know.

(define bits-used-per-byte 8)

(define byte-limit (expt 2 bits-used-per-byte))
(define two-byte-limit (* byte-limit byte-limit))

; Bytecodes: for compiler and interpreter

; Instruction specification is 
; (op . args)
; OP may be a name or a list of names
; ARGS are
;  nargs        - a byte
;  byte         - a byte
;  two-bytes    - two bytes
;  literal      - a one-byte value
;  index        - a two byte index into the current template or environment
;  small-index  - a one byte index into the current template or environment
;  offset       - two bytes giving an offset into the current instruction stream
;  offset-      - same thing, negative offset
;  stob         - a byte specifying a type for a stored object
;  env-data     - environment specification with one-byte values
;  big-env-data - environment specification with two-byte values
;  moves-data   - specification of stack shuffle moves
;  big-moves-data - specification of stack shuffle moves
;  0 1 2 ...    - the number of non-instruction-stream arguments (some
;                 instructions take a variable number of arguments; the first
;                 number is the argument count implemented by the VM)
;  +            - any number of additional arguments are allowed

(define-syntax define-instruction-set
  (lambda (form rename compare)
    (let ((data (do ((data (reverse (cdr form)) (cdr data))
		     (new '() (let ((next (car data)))
				(if (pair? (car next))
				    (append (map (lambda (op)
						   (cons op (cdr next)))
						 (car next))
					    new)
				    (cons next new)))))
		    ((null? data) new))))
      `(begin (define-enumeration op
		,(map car data))
	      (define opcode-arg-specs
		'#(,@(map cdr data)))))))

; Instructions marked *EXP* are experimental and are not normally used by
; byte-code compiler.

(define-instruction-set
  (protocol       protocol)      ; first opcode in a procedure, never actually
				 ; executed

  (integer-literal      literal) ; optimization for one-byte integers (also
				 ; used in the closed-compiled +, *)
  (push+integer-literal literal) ; preceded by a push
  (integer-literal+push literal) ; followed by a push

  (global      two-bytes index)   ; first is template, second within template
  (set-global! two-bytes index 1) ; first is template, second within template

  (make-flat-env  env-data)      ; make new environment from env-data
  (make-big-flat-env big-env-data) ; same, but with two-byte size and offsets

  (push 1)		         ; push *val* onto stack
  (push-false)			 ; a common combination
  (pop)			         ; pop top of stack into *val*
  (pop-n	  two-bytes)     ; remove the top N values from the stack
				 ; leaving *val* unchanged
  (push-n	  two-bytes)     ; allocate space for N values on stack
				 ; leaving *val* unchanged
  (stack-ref      byte)	         ; index'th element of stack into *val*
  (push+stack-ref byte)	         ; preceded by a push
  (stack-ref+push byte)	         ; followed by a push
  (big-stack-ref  two-bytes)
  (stack-set!     byte 1)        ; *val* to index'th element of stack
  (big-stack-set! two-bytes 1)
  (stack-indirect byte byte)     ; first is index into stack, second is index
			  	 ; into what you find there
  (push+stack-indirect byte byte) ; preceded by a push
  (stack-indirect+push byte byte) ; followed by a push
  (big-stack-indirect two-bytes two-bytes)

  (stack-shuffle! moves-data)	 ; shuffle stack elements around
  (big-stack-shuffle! big-moves-data)	 ; shuffle stack elements around with two-byte offsets

  (current-cont)	         ; copy *cont* to *val*, use WITH-CONTINUATION
			         ; to use copied continuation
  (cont-data      offset)	 ; offset of next instruction; never executed

  ;; Different ways to call procedures
  (call     offset nargs 1 +)    ; last argument is the procedure to call,
				 ; offset is to return pointer
  (tail-call nargs 1 +)          ; same, no return pointer, moves arguments
  (big-call offset two-bytes 1 +) ; ditto, nargs counts are two bytes
  (poll offset)			 ; offset is for continuation-pc if interrupt
  (apply offset two-bytes 2 +)   ; last argument is procedure to call, second to
				 ; last is a list of additional arguments, next
                                 ; two bytes are the number of stack arguments
  (closed-apply 2 +)		 ; arguments are as for Scheme's APPLY, with
				 ; the number of non-list arguments pushed on
  				 ; the top of the stack
  (with-continuation 2)		 ; first arg is cont, second is procedure

  ;; Three different ways to return from calls
  (return 1)			 ; return one value
  (values two-bytes +)		 ; values are on stack, count is next two bytes
  (closed-values +)		 ; values are on stack, count is pushed on stack

  ;; Six different ways to jump
  (goto-template index)		 ; jump to another template (*EXP*)
				 ; does not poll for interrupts
  (call-template offset index index nargs)
				 ; call a template instead of a procedure
				 ; nargs is needed for interrupt handling
  (jump-if-false offset 1)	 ; boolean in *val*
  (jump          offset)
  (jump-back     offset-)	 ; same, but subtract the offset
  (computed-goto byte offset 1)	 ; jump using delta specified by *val*
				 ; defaults to instruction after deltas (*EXP*)

  ;; For the closed-compiled definitions of n-ary arithmetic functions.
  ;; The opcode sequences used are:
  ;;       binary-reduce1 binary-op binary-reduce2 return
  ;; and
  ;;       binary-reduce1 binary-op binary-comparison-reduce2 return
  ((binary-reduce1 binary-reduce2 binary-comparison-reduce2))

  ;; Scalar primitives
  (eq? 2)

  ((number? integer? rational? real? complex? exact?) 1)
  ((exact->inexact inexact->exact) 1)

  ((+ *) 2 0 1 +)
  ((- /) 2 1)
  ((= < > <= >=) 2 +)
  ((quotient remainder) 2)
  ((floor numerator denominator
     real-part imag-part
     exp log sin cos tan asin acos sqrt
     angle magnitude)
   1)
  (atan1 1)
  (atan2 2)
  ((make-polar make-rectangular) 2)
  (bitwise-not 1)
  (bit-count 1)
  ((bitwise-and bitwise-ior bitwise-xor) 2)
  (arithmetic-shift 2)
  (char? 1)
  ((char=? char<?) 2)
  ((char->ascii ascii->char) 1)
  (eof-object? 1)

  ;; Data manipulation
  (stored-object-has-type? stob 1)
  (stored-object-length stob 1)

  (make-stored-object byte stob)
  (closed-make-stored-object stob)	; size pushed on stack
  (stored-object-ref  stob byte 1)	; byte is the offset
  (stored-object-set! stob byte byte 2)	; byte0 is the offset, byte1 = 0
  					; means not to log in the proposal

  (make-vector-object stob 2)			; size + init
  ; If the byte = 0 then do not log in the current proposal
  (stored-object-indexed-ref  stob byte 2)	; vector + offset
  (stored-object-indexed-set! stob byte 3)	; vector + offset + value

  (make-byte-vector 2)
  (byte-vector-length 1)
  (byte-vector-ref 2)
  (byte-vector-set! 3)

  (make-string 2)
  (string-length 1)
  (string-ref 2)
  (string-set! 3)

  (intern 1)
                     
  (location-defined? 1)
  (set-location-defined?! 2)
  ((immutable? make-immutable!) 1)

  ;; channels (unbuffered, non-blocking I/O)
  (open-channel 2)
  (close-channel 1)
  (channel-maybe-read 5)
  (channel-maybe-write 4)
  (channel-ready? 1)
  (channel-abort 1)             ; stop channel operation
  (open-channels-list)		; return a list of the open channels
  
  ;; Optimistic concurrency
  (current-proposal)
  (set-current-proposal! 1)
  (maybe-commit)
  (stored-object-logging-ref stob byte 1)
  (copy-bytes! byte 5)		; byte = 0 -> don't log
  (byte-vector-logging-ref 2)
  (byte-vector-logging-set! 3)

  ;; Misc
  ((unassigned unspecific))
  (trap 1)			; raise exception
  (false)			; return #f (for bootstrapping)
  (eof-object)                  ; hard to get otherwise
  (write-image-low 4)
  (collect)
  (string-hash 1)		; used by the static linker for the initial table
  (add-finalizer! 2)
  (memory-status 2)
  (find-all 1)	                ; makes a vector of all objects of a given type
  (find-all-records 1)          ; makes a vector of all records of a given type
  (current-thread)
  (set-current-thread! 1)
  (session-data)                ; session specific data
  (set-session-data! 1)
  (set-exception-handlers! 1)
  (return-from-exception 1)
  (return-from-native-exception 1)
  (set-interrupt-handlers! 1)
  (set-enabled-interrupts! 1)
  (return-from-interrupt)
  (schedule-interrupt 1)
  (wait 2)                      ; do nothing until something happens
  (call-external-value 1 +)
  (lookup-shared-binding 2)
  (undefine-shared-binding 2)
  (time 2)
  (vm-extension 2)		; access to extensions of the virtual machine
  (return-from-callback 2)	; return from an callback

  ;; Unnecessary primitives
  (string=? 2)
  (reverse-list->string 2)
  (assq 2)
  (unassigned-check 1)
  ; If the byte = 0 then do not log in the current proposal
  (checked-record-ref byte 3)
  (checked-record-set! byte 4)

  ;; ports (buffered I/O) - these are all unnecessary
  ;; byte = 0 -> port is supplied
  ;;      = 1 -> get port from dynamic environment
  ((read-char peek-char) byte 1 0)
  (write-char byte 2 1)

  ;; For writing informative messages when debugging
  (message 1)
  )

(define-enumeration interrupt
  (alarm           ; order matters - higher priority first
   keyboard
   post-gc         ; handler is passed a list of finalizers
   i/o-completion  ; handler is passed channel and status
   os-signal
   ))

; Possible problems

(define-enumeration exception
  (unassigned-local
   undefined-global
   unbound-global
   bad-procedure
   wrong-number-of-arguments
   wrong-type-argument
   arithmetic-overflow
   index-out-of-range
   heap-overflow
   out-of-memory
   cannot-open-channel
   channel-os-index-already-in-use
   closed-channel
   buffer-full/empty
   unimplemented-instruction
   trap
   proceeding-after-exception
   bad-option
   unbound-external-name
   too-many-arguments-to-external-procedure
   too-many-arguments-in-callback
   callback-return-uncovered
   extension-exception
   extension-return-error
   os-error
   gc-protection-mismatch
   no-current-proposal
   native-code-not-supported
   illegal-exception-return
   ))

; Used by (READ-CHAR) and (WRITE-CHAR) to get the appropriate ports from
; the fluid environment.

(define-enumeration current-port-marker
  (current-input-port
   current-output-port))

;----------------
; Call and return protocols
;
; Protocols 0..maximum-stack-args are just the number of arguments sitting on
; the stack.

(define maximum-stack-args 63)

(define *last-protocol* maximum-stack-args)

(define (next-protocol)
  (set! *last-protocol* (+ *last-protocol* 1))
  *last-protocol*)

; The next two bytes gives the expected number of arguments.

(define two-byte-nargs-protocol      (next-protocol))

; Used for all n-ary procedures.  The next two bytes gives the minimum
; number of arguments.

(define two-byte-nargs+list-protocol (next-protocol))

; Drop any and all arguments on the floor.  The compiler doesn't use this
; for procedures, but does generate it for continuations.

(define ignore-values-protocol (next-protocol))

; Real protocol is at the end of the code vector, along with the required
; stack size:
;   ... real-protocol stack-size0 stack-size1
; This stuff has to be at the end of the code vector because the necessary stack
; size is not determined until after the code vector has been assembled.

(define big-stack-protocol (next-protocol))

; The rest are used only for the definitions of various Scheme primitives.

; For VECTOR, RECORD, VALUES, EXTERNAL-CALL, APPLY
; Next byte is the minimum number of arguments (1 for EXT-CALL, 2 for APPLY,
; 0 for the rest).
; Stack = arg0 arg1 ... argN rest-list N+1 total-arg-count
; The first two arguments are always on the stack.

(define args+nargs-protocol (next-protocol))

; Followed by four bytes: the offsets of code for the 3+, 0, 1, and 2 arg cases.
; A zero indicates that the primitive doesn't accept that number of arguments.
; If there are fewer than three arguments they are all on the stack.  In the
; 3+ case the setup is the same as args+nargs above (it's first so that it can
; share code in the VM with args+nargs).

(define nary-dispatch-protocol (next-protocol))

; The following is used to mark continuations created for the first argument
; to CALL-WITH-VALUES when the second argument is not a LAMBDA expression.
; The continuation contains a single value, the procedure to be called on the
; values returned by the first argument.

(define call-with-values-protocol (next-protocol))

; Used to mark the continuation at the bottom of the stack cash.

(define bottom-of-stack-protocol (next-protocol))

; Native protocols are the same, except with the high bit set.

(define native-protocol-mask #x80)

; The maximum number of arguments that can be passed to EXTERNAL-CALL.
; This is determined by the C procedure `external_call()'.

(define maximum-external-call-args 12)

;----------------
; The number of stack slots available to each procedure by default.
; Procedures that need more than this must use one of the two-byte-nargs
; protocols.  All of these are given in terms of descriptors.

(define default-stack-space 64)

(define continuation-stack-size 4)  ; header + continuation + pc + code

(define available-stack-space 8000) ; how much stack space is available for
                                    ; any one procedure

; The number of values that the VM adds to continuations.
(define continuation-cells 3)

; Offsets of saved registers in continuations
(define continuation-pc-index   0)
(define continuation-code-index 1)
(define continuation-cont-index 2)

; The number of additional values that the VM adds to exception continuations.
(define exception-continuation-cells 5)

; Offsets of saved registers in exception continuations.  Size must come
; first because the VM expects it there.
(define exception-cont-size-index             (+ continuation-cells 0))
(define exception-cont-pc-index               (+ continuation-cells 1))
(define exception-cont-code-index             (+ continuation-cells 2))
(define exception-cont-exception-index        (+ continuation-cells 3))
(define exception-cont-instruction-size-index (+ continuation-cells 4))

;----------------

; Options for op/time

(define-enumeration time-option
  (run-time
   real-time
   cheap-time     ; cheap (no system call) access to the polling clock
   ;current-time
   ))

; Options for op/memory-status

(define-enumeration memory-status-option
  (available
   heap-size
   stack-size
   gc-count
   expand-heap!
   pointer-hash
   ))

; The two types of special channels cannot be used for normal I/O.

(define-enumeration channel-status-option
  (closed
   input
   output
   special-input   ; socket accept, ???
   special-output  ; ???
   ))

; Indicies into a port's status word

(define-enumeration port-status-options
  (input
   output
   open-for-input
   open-for-output
   ))

(define-enumeration stob
  (;; D-vector types (traced by GC)
   pair
   symbol
   vector
   closure
   location
   cell
   channel
   port
   ratnum
   record
   continuation
   extended-number
   template
   weak-pointer
   shared-binding
   unused-d-header1
   unused-d-header2

   ;; B-vector types (not traced by GC)
   string        ; = least b-vector type
   byte-vector
   double        ; double precision floating point
   bignum
   ))

; This is here to try to ensure that it is changed when STOB changes.
(define least-b-vector-type (enum stob string))

; (stob predicate constructor . (accessor modifier)*)
; If nothing else, the run-time system and the VM need to agree on
; which slot of a pair is the car and which is the cdr.

(define stob-data
  '((pair pair? cons
      (car set-car!) (cdr set-cdr!))
    (symbol symbol? #f       ; RTS calls op/string->symbol
      (symbol->string))
    (location location? make-location
      (location-id set-location-id!)
      (contents set-contents!))
    (cell cell? make-cell
      (cell-ref cell-set!))
    (closure closure? make-closure
      (closure-template) (closure-env))
    (weak-pointer weak-pointer? make-weak-pointer
      (weak-pointer-ref))
    (shared-binding shared-binding? make-shared-binding
      (shared-binding-name)
      (shared-binding-is-import?)
      (shared-binding-ref shared-binding-set!))
    (port port? make-port
      (port-handler)
      (port-status  set-port-status!)
      (port-lock    set-port-lock!)		; used for buffer timestamps
      (port-data    set-port-data!)
      (port-buffer  set-port-buffer!)
      (port-index   set-port-index!)
      (port-limit   set-port-limit!)
      (port-pending-eof? set-port-pending-eof?!))
    (channel channel? #f
      (channel-status)
      (channel-id)
      (channel-os-index))
    ))

