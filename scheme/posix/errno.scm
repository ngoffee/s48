; Copyright (c) 1993-2008 by Richard Kelsey and Jonathan Rees. See file COPYING.

; Error codes

(import-dynamic-externals "=scheme48external/posix")

(define-record-type unnamed-errno :unnamed-errno
  (unnamed-errnos-are-made-by-c-code)
  unnamed-errno?
  (resume-value unnamed-errno-resume-value)
  (os-number    unnamed-errno-os-number))

(define-record-discloser :unnamed-errno
  (lambda (u-s)
    (list 'errno (unnamed-errno-os-number u-s))))

; These are not meaningful after a dump (because the value may not have the
; same meaning on the OS on which we are resumed).

(define-record-resumer :unnamed-errno #f)

(define-finite-type errno :named-errno ()
  named-errno?
  named-errnos
  named-errno-name
  named-errno-index
  (os-number named-errno-os-number set-named-errno-os-number!)
  (					; POSIX
   (toobig)			     ; [E2BIG] Argument list too long.
   (acces)				; Permission denied.
   (addrinuse)				; Address in use.
   (addrnotavail)			; Address not available.
   (afnosupport)		       ; Address family not supported.
   (again) ; Resource unavailable, try again (may be the same value as [EWOULDBLOCK]).
   (already)			     ; Connection already in progress.
   (badf)				; Bad file descriptor.
   (badmsg)				; Bad message.
   (busy)				; Device or resource busy.
   (canceled)				; Operation canceled.
   (child)				; No child processes.
   (connaborted)			; Connection aborted.
   (connrefused)			; Connection refused.
   (connreset)				; Connection reset.
   (deadlk)			      ; Resource deadlock would occur.
   (destaddrreq)		       ; Destination address required.
   (dom)	     ; Mathematics argument out of domain of function.
   (dquot)				; Reserved.
   (exist)				; File exists.
   (fault)				; Bad address.
   (fbig)				; File too large.
   (hostunreach)			; Host is unreachable.
   (idrm)				; Identifier removed.
   (ilseq)				; Illegal byte sequence.
   (inprogress)				; Operation in progress.
   (intr)				; Interrupted function.
   (inval)				; Invalid argument.
   (io)					; I/O error.
   (isconn)				; Socket is connected.
   (isdir)				; Is a directory.
   (loop)			  ; Too many levels of symbolic links.
   (mfile)				; Too many open files.
   (mlink)				; Too many links.
   (msgsize)				; Message too large.
   (multihop)				; Reserved.
   (nametoolong)			; Filename too long.
   (netdown)				; Network is down.
   (netreset)			      ; Connection aborted by network.
   (netunreach)				; Network unreachable.
   (nfile)			      ; Too many files open in system.
   (nobufs)				; No buffer space available.
   (nodata) ; [XSR]  No message is available on the STREAM head read queue. 
   (nodev)				; No such device.
   (noent)				; No such file or directory.
   (noexec)			       ; Executable file format error.
   (nolck)				; No locks available.
   (nolink)				; Reserved.
   (nomem)				; Not enough space.
   (nomsg)			     ; No message of the desired type.
   (noprotoopt)				; Protocol not available.
   (nospc)				; No space left on device.
   (nosr)				; [XSR]  No STREAM resources. 
   (nostr)				; [XSR]  Not a STREAM. 
   (nosys)				; Function not supported.
   (notconn)				; The socket is not connected.
   (notdir)				; Not a directory.
   (notempty)				; Directory not empty.
   (notsock)				; Not a socket.
   (notsup)				; Not supported.
   (notty)			; Inappropriate I/O control operation.
   (nxio)				; No such device or address.
   (opnotsupp)			  ; Operation not supported on socket.
   (overflow)		  ; Value too large to be stored in data type.
   (perm)				; Operation not permitted.
   (pipe)				; Broken pipe.
   (proto)				; Protocol error.
   (protonosupport)			; Protocol not supported.
   (prototype)			     ; Protocol wrong type for socket.
   (range)				; Result too large.
   (rofs)				; Read-only file system.
   (spipe)				; Invalid seek.
   (srch)				; No such process.
   (stale)				; Reserved.
   (time)			     ; [XSR]  Stream ioctl() timeout. 
   (timedout)				; Connection timed out.
   (txtbsy)				; Text file busy.
   (wouldblock)	; Operation would block (may be the same value as [EAGAIN]).
   (xdev)				; Cross-device link.
   ))

(define-record-discloser :named-errno
  (lambda (n-s)
    (list 'errno (named-errno-name n-s))))

; Find the errno called `name'.

(define (name->errno name)
  (if (not (symbol? name))
      (assertion-violation 'name->errno "argument not a symbol" name)
      (let loop ((i 0))
	(cond ((= i (vector-length named-errnos))
	       #f)
	      ((eq? name
		    (named-errno-name
		      (vector-ref named-errnos i)))
	       (vector-ref named-errnos i))
	      (else
	       (loop (+ i 1)))))))

;----------------
; Code to produce a C include file that checks whether each errno is defined.
; The output file looks like:
;
; errno_count_is(<number of errnos>);
; #ifdef E2BIG
; errno_map[0] = E2BIG;
; #endif
; #ifdef EACCESS
; errno_map[1] = EACCESS;
; #endif
; ...

(define (write-c-errno-include-file filename)
  (call-with-output-file filename
    (lambda (out)
      (display (string-append "errno_count_is("
			      (number->string (vector-length named-errnos))
			      ");"
			      newline-string)
	       out)
      (do ((i 0 (+ i 1)))
	  ((= i (vector-length named-errnos)))
	(let* ((name (named-errno-name
		      (vector-ref named-errnos i)))
	       (posix-name (if (eq? name 'toobig)
			       "2BIG" ; argl
			       (symbol->string name))))
	  (display (string-append "#ifdef E" (string-upcase posix-name)
				  newline-string
				  "errno_map["
				  (number->string i)
				  "] = E"
				  (string-upcase posix-name)
				  ";"
				  newline-string
				  "#endif"
				  newline-string)
		   out))))))

(define newline-string (list->string '(#\newline)))

(define (string-map proc)
  (lambda (list)
    (list->string (map proc (string->list list)))))

;----------------
; Dispatching on the two kinds of errnos.

(define (errno? x)
  (or (named-errno? x)
      (unnamed-errno? x)))

(define (errno-name x)
  (cond ((named-errno? x)
	 (named-errno-name x))
	((unnamed-errno? x)
	 #f)
	(else
	 (assertion-violation 'errno-name "argument not a errno" x))))

(define (errno-os-number x)
  (cond ((named-errno? x)
	 (named-errno-os-number x))
	((unnamed-errno? x)
	 (unnamed-errno-os-number x))
	(else
	 (assertion-violation 'errno-os-number "argument not a errno" x))))

; Two errnos are the same if they are exactly the same or if they are
; both named errnos and have the same (non-#F) os number.

(define (errno=? s1 s2)
  (or (eq? s1 s2)
      (and (named-errno? s1)
	   (named-errno? s2)
	   (named-errno-os-number s1)
	   (eq? (named-errno-os-number s1)
		(named-errno-os-number s2)))))

;----------------
; What we contribute to and receive from the C layer.

(define-exported-binding "posix-errnos-vector"        named-errnos)
(define-exported-binding "posix-named-errno-type"     :named-errno)
(define-exported-binding "posix-unnamed-errno-type"   :unnamed-errno)
(define-exported-binding "posix-unnamed-errno-marker" 'nonportable-errno)

(import-lambda-definition-2 integer->errno (int) "posix_integer_to_errno")
(import-lambda-definition-2 initialize-named-errnos ()
			  "posix_initialize_named_errnos")

;----------------
; A vector mapping os-errno numbers to errnos and add to it any errnos
; that have existing errno queues.

(define os-errno-map (make-session-data-slot! #f))

; Initializing the above vector.

(define (initialize-errnos)
  (let ((ints (set-enabled-interrupts! no-interrupts)))
    (initialize-named-errnos)
    (let* ((named (vector->list named-errnos))
	   (size (+ 1 (apply max
			     (map (lambda (errno)
				    (or (errno-os-number errno)
					-1))
				  named))))
	   (mapper (make-vector size '())))
      (for-each (lambda (errno)
		  (cond
		   ((errno-os-number errno)
		    => (lambda (number)
			 (let ((old (vector-ref mapper number)))
			   (vector-set! mapper number (cons errno old)))))))
		named)
      (session-data-set! os-errno-map mapper)
      (set-enabled-interrupts! ints))))

;----------------
; Initialize errnos now ...

(initialize-errnos)

; ... and on later startups.

(define-reinitializer errno-reinitializer initialize-errnos)
