; Copyright (c) 1993, 1994 Richard Kelsey and Jonathan Rees.  See file COPYING.


; File processing, etc. for compiler


; Compile-form and compile-file return templates, suitable for
;  execution via (invoke-closure (make-closure <template> <arbitrary>)).

(define (compile-form form p)
  (compile-scanned-forms (scan-forms (list form) p #f)
			 p #f #f))


; Compile a file.  The entire file is first scanned to process all
; definitions.  Then it's compiled.  The result is a template.

(define (compile-file filename p)	; => thunk template
  (let* ((scanned-forms (scan-file filename p))
	 (o-port (current-output-port))
	 (template (compile-scanned-forms scanned-forms p filename o-port)))
    (newline o-port)
    (force-output o-port)
    template))


; compile-and-run-{form,file} are the same as compile-{form, file},
; but instead of returning templates, they produce a template for each
; form as the form is compiled.  Each template is passed to a
; specified action procedure.

(define (compile-and-run-forms forms p filename run noisy?)
  (compile-and-run-scanned-forms (scan-forms forms p filename)
				 p filename run noisy?))

(define (compile-and-run-file filename p run noisy?)
  (compile-and-run-scanned-forms (scan-file filename p) p filename run
				 noisy?))

; --------------------

; If non-noisy, this tail-recurs to last form.

(define (compile-and-run-scanned-forms scanned-forms p filename run noisy?)
  (let* ((do-it (lambda (scanned-form)
		  (compile-scanned-forms (list scanned-form)
					 p filename noisy?)))
	 (do-them (lambda ()
		    (if (null? scanned-forms)
			(compile-scanned-forms '() p #f #f)
			(do-it (do ((scanned-forms scanned-forms
						   (cdr scanned-forms)))
				   ((null? (cdr scanned-forms))
				    (car scanned-forms))
				 (run (do-it (car scanned-forms)))))))))
    (if noisy?
	(call-with-values (lambda ()
			    (run (do-them)))
	  (lambda results
	    (newline noisy?)
	    (force-output noisy?)
	    (apply values results)))
	(run (do-them)))))


; compile-scanned-forms: returns a template.

(define (compile-scanned-forms scanned-forms p filename noisy? . env-option)
  (let ((cenv (bind-source-file-name
	       filename
	       (if (null? env-option)
		   (package->environment p)
		   (car env-option)))))
    (segment->template
     (if (null? scanned-forms)
	 (deliver-value (instruction (enum op unspecific))
			(return-cont #f))
	 (let recur ((scanned-forms scanned-forms))
	   (if (null? (cdr scanned-forms))
	       (compile-scanned-form (car scanned-forms) cenv
				     (return-cont #f) noisy?)
	       (careful-sequentially 
		(compile-scanned-form (car scanned-forms) cenv
				      an-ignore-values-cont noisy?)
		;; Cf. compile-begin
		(recur (cdr scanned-forms))
		0
		(return-cont #f)))))
     filename
     #f)))				;pc-in-segment = #f

; Compile a single top-level form, returning a segment.

(define (compile-scanned-form node cenv cont noisy?)
  (cond ((define-node? node)
	 (let ((segment (compile-definition node cenv cont noisy?)))
	   (if noisy?
	       (begin (write-char #\. noisy?)
		      (force-output noisy?)))
	   segment))
	((define-syntax-node? node)
	 (deliver-value (instruction (enum op unspecific)) cont))
	(else
	 (compile-top node cenv 0 cont))))


; Definitions must be treated differently from assignments: we must
; use SET-CONTENTS! instead of SET-GLOBAL! because the SET-GLOBAL!
; instruction traps if an attempt is made to store into an undefined
; location.

(define compile-definition
  (let ((location-contents-offset 0))	;should be found in stob-data
    (lambda (node cenv cont noisy?)
      (let* ((form (node-form node))
	     (name (cadr form))
	     (loc (binding-place (lookup cenv name))))
	(sequentially (instruction-with-location (enum op literal)
						 (lambda () loc))
		      (instruction (enum op push))
		      (compile-top (caddr form)
				   cenv
				   1
				   (named-cont name))
		      (deliver-value
		       (instruction (enum op stored-object-set!)
				    (enum stob location)
				    location-contents-offset)
		       cont))))))

; --------------------
; Hairy stuff for dealing with undefined variables.

; GET-LOCATION returns a thunk that will obtain a location to be
; stored away in a template.  The hair here results from a desire to
; be able to recover from errors.

(define (get-location binding cenv name want-type)
  (if (binding? binding)
      (let ((win (lambda ()
		   (note-caching cenv name (binding-place binding)))))
	(if (compatible-types? (binding-type binding) want-type)
	    win
	    (if (variable-type? want-type)
		(get-location-for-unassignable cenv name)
		(begin (warn "invalid variable reference" name)
		       win))))
      (get-location-for-undefined cenv name)))

(define (get-location-for-undefined cenv name)
  (if (generated? name)
      (get-location-for-undefined (generated-env name)
				  (generated-symbol name))
      (let ((p (grumble cenv)))
	(note-undefined! p name)
	(if (package? p)
	    (lambda ()
	      (package-note-caching p name
				    (location-for-reference p name)))
	    (lambda ()
	      (make-undefined-location name))))))
		    
(define (get-location-for-unassignable cenv name)
  (if (generated? name)
      (get-location-for-unassignable (generated-env name)
				     (generated-symbol name))
      (let ((p (grumble cenv)))
	(warn "invalid assignment" name)
	(if (package? p)
	    (lambda () (location-for-assignment p name))
	    (lambda () (make-undefined-location name))))))

(define (package-accessor-on-demand id init)
  (lambda (p)
    (or (package-get p id)
	(let ((foo (init)))
	  (package-put! p id foo)
	  foo))))

(define (location-on-demand accessor)
  (lambda (p name)
    (let ((table (accessor p)))
      (or (table-ref table name)
	  (let ((new (make-new-location p name)))
	    (table-set! table name new)
	    new)))))

; Create undefined variable -> location tables on demand.

(define package-undefineds
  (package-accessor-on-demand 'undefineds
			      (lambda () (make-table name-hash))))

(define location-for-reference
  (let ((get-undefined (location-on-demand package-undefineds)))
    (lambda (p name)
      (let loop ((opens (package-opens p)))
	(if (null? opens)
	    (get-undefined p name)
	    (if (interface-ref (structure-interface (car opens))
			       name)
		(location-for-reference (structure-package (car opens)) name)
		(loop (cdr opens))))))))

(define package-undefined-but-assigneds
  (package-accessor-on-demand 'undefined-but-assigneds
			      (lambda () (make-table name-hash))))

(define location-for-assignment
  (location-on-demand package-undefined-but-assigneds))


(define (note-caching cenv name place)
  (if (generated? name)
      (note-caching (generated-env name)
		    (generated-symbol name)
		    place)
      (let ((p (grumble cenv)))
	(if (package? p)
	    (package-note-caching p name place)
	    place))))

(define (grumble cenv)
  (cond ((procedure? cenv)
	 ;; This returns #f if package is stable (static linking).
	 (extract-package-from-environment cenv))
	((package? cenv) cenv)
	((structure? cenv) (structure-package cenv))))

; --------------------
; Make a startup procedure from a list of initialization thunks.  This
; is only used by the static linker.

(define (make-startup-procedure inits resumer)
  (let ((nargs 3))
    (segment->template
      (reduce (lambda (init seg)
		(sequentially
		 (maybe-push-continuation
		  (sequentially
		   (instruction-with-literal (enum op closure) init)
		   (instruction (enum op call) 0))
		  nargs
		  an-ignore-values-cont)
		 seg))
	      (sequentially
	       (maybe-push-continuation
		  (sequentially
		   (instruction-with-literal (enum op closure) resumer)
		   (instruction (enum op call) 0))
		  nargs
		  (fall-through-cont #f #f))
	       ;; was (compile resumer p nargs (fall-through-cont))
	       (instruction (enum op call) nargs))
	      inits)
      #f #f)))

(define an-ignore-values-cont (ignore-values-cont #f #f))
